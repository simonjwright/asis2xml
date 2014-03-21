--  Copyright 2004 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  Part of ASIS2XML.
--
--  $Id$
--
--  Developed from the Node_Trav component of Display_Source, which is
--  distributed as a part of the ASIS implementation for GNAT and is
--  Copyright (c) 1995-1999, Free Software Foundation, Inc.

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);

with Asis.Compilation_Units;
with Asis.Data_Decomposition;
with Asis.Declarations;
with Asis.Elements;
with Asis.Expressions;
with Asis.Iterator;
with Asis.Text;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

package body XML_Support is

   function "+" (Item : String) return Wide_String
     renames Ada.Characters.Handling.To_Wide_String;

   function "+" (Item : Wide_String) return String;
   function "+" (Item : Wide_String) return String is
   begin
      return Ada.Characters.Handling.To_String (Item);
   end "+";

   function To_Tag_Name (Image : String) return String;
   function To_Tag_Name (Image : String) return String is
      Result : String := Ada.Characters.Handling.To_Lower (Image);
   begin
      if Result (1 .. 2) = "a_" then
         return Result (3 .. Result'Last);
      elsif Result (1 .. 3) = "an_" then
         return Result (4 .. Result'Last);
      else
         return Result;
      end if;
   end To_Tag_Name;

   --  Traits (see Asis spec) are an additional classification
   --  mechanism that lets Asis use fewer high-level elements. Not
   --  every A_Definition element supports traits, and not every trait
   --  is applicable in all circumstances. This code assumes that the
   --  Asis tree is well-formed, so if a trait is present it is legal
   --  and we just add the corresponding attribute to the DOM tree.
   procedure Add_Trait (K : Asis.Trait_Kinds; To : DOM.Core.Node);
   procedure Add_Trait (K : Asis.Trait_Kinds; To : DOM.Core.Node) is
   begin
      case K is
         when Asis.Not_A_Trait | Asis.An_Ordinary_Trait => null;
         when Asis.An_Aliased_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "aliased", "true");
         when Asis.An_Access_Definition_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "access", "true");
         when Asis.A_Reverse_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "reverse", "true");
         when Asis.A_Private_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "private", "true");
         when Asis.A_Limited_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "limited", "true");
         when Asis.A_Limited_Private_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "limited", "true");
            DOM.Core.Elements.Set_Attribute (To, "private", "true");
         when Asis.An_Abstract_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "abstract", "true");
         when Asis.An_Abstract_Private_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "abstract", "true");
            DOM.Core.Elements.Set_Attribute (To, "private", "true");
         when Asis.An_Abstract_Limited_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "abstract", "true");
            DOM.Core.Elements.Set_Attribute (To, "limited", "true");
         when Asis.An_Abstract_Limited_Private_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "abstract", "true");
            DOM.Core.Elements.Set_Attribute (To, "limited", "true");
            DOM.Core.Elements.Set_Attribute (To, "private", "true");
     end case;
   end Add_Trait;

   --  Add a 'unit' attribute containing E's unit name, if the
   --  declaration E is in fact a unit.
   --
   --  Could probably arrange to call this only when if could succeed.
   procedure Add_Unit_Info (E : Asis.Element; To : DOM.Core.Node);
   procedure Add_Unit_Info (E : Asis.Element; To : DOM.Core.Node) is
      Unit : constant Asis.Compilation_Unit
        := Asis.Elements.Enclosing_Compilation_Unit (E);
      Unit_Decl : constant Asis.Declaration
        := Asis.Elements.Unit_Declaration (Unit);
   begin
      if Asis.Elements.Is_Equal (E, Unit_Decl) then
         DOM.Core.Elements.Set_Attribute
           (To,
            "unit",
            +Asis.Compilation_Units.Unit_Full_Name (Unit));
         DOM.Core.Elements.Set_Attribute
           (To,
            "file",
            +Asis.Compilation_Units.Text_Name (Unit));
      end if;
   end Add_Unit_Info;

   procedure Initialize (XI : in out Info;
                         Root : Asis.Element;
                         Document : DOM.Core.Node) is
   begin
      XI.Root := Root;
      XI.Document := Document;
      XI.Current := DOM.Core.Nodes.Append_Child
        (XI.Document,
         DOM.Core.Documents.Create_Element (XI.Document, "asis"));
   end Initialize;

   procedure Finalize (XI : in out Info) is
   begin
      null;
   end Finalize;

   procedure Pre (Element : in Asis.Element;
                  Control : in out Asis.Traverse_Control;
                  State : in out Info) is
      Tmp : DOM.Core.Node;
      use Asis;
      use type DOM.Core.Node;
   begin

--        Put_Line ("Pre (" & Asis.Elements.Element_Kind (Element)'Img & ")");
      case Asis.Elements.Element_Kind (Element) is

         when A_Pragma =>                  -- Asis.Elements
            State.Current :=
              DOM.Core.Nodes.Append_Child
              (State.Current,
               DOM.Core.Documents.Create_Element
                 (State.Document,
                  To_Tag_Name
                       (Pragma_Kinds'Image
                          (Asis.Elements.Pragma_Kind (Element)))));
            Tmp :=
              DOM.Core.Nodes.Append_Child
              (State.Current,
               DOM.Core.Documents.Create_Text_Node
                 (State.Document,
                  +Asis.Elements.Pragma_Name_Image (Element)));

         when A_Defining_Name =>           -- Asis.Declarations
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                       (Defining_Name_Kinds'Image
                          (Asis.Elements.Defining_Name_Kind (Element)))));
            Tmp :=
              DOM.Core.Nodes.Append_Child
              (State.Current,
               DOM.Core.Documents.Create_Text_Node
                 (State.Document,
                  +Asis.Declarations.Defining_Name_Image (Element)));

            case Asis.Elements.Defining_Name_Kind (Element) is
               when A_Defining_Operator_Symbol =>
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "operator_kind",
                     To_Tag_Name
                       (Operator_Kinds'Image
                          (Asis.Elements.Operator_Kind (Element))));
               when others => null;
            end case;

         when A_Declaration =>             -- Asis.Declarations
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Declaration_Kinds'Image
                     (Asis.Elements.Declaration_Kind (Element)))));

            Add_Unit_Info (Element, State.Current);

            --  Trait handling
            case Asis.Elements.Declaration_Kind (Element) is
               when A_Private_Type_Declaration =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Private_Extension_Declaration =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Variable_Declaration =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Constant_Declaration =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Deferred_Constant_Declaration =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Discriminant_Specification =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Loop_Parameter_Specification =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Procedure_Declaration |
                 A_Function_Declaration =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
               when A_Parameter_Specification =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);
                  case Asis.Elements.Mode_Kind (Element) is
                     when An_In_Mode =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "mode", "in");
                     when An_Out_Mode =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "mode", "out");
                     when An_In_Out_Mode =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "mode", "inout");
                     when others => null;
                  end case;
               when A_Formal_Object_Declaration =>
                  case Asis.Elements.Mode_Kind (Element) is
                     when An_In_Mode =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "mode", "in");
                     when An_Out_Mode =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "mode", "out");
                     when An_In_Out_Mode =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "mode", "inout");
                     when others => null;
                  end case;
               when A_Formal_Procedure_Declaration |
                 A_Formal_Function_Declaration =>
                  --  Should be Subprogram_Default_Kind
                  case Asis.Elements.Default_Kind (Element) is
                     when A_Name_Default =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "default", "name");
                     when A_Box_Default =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "default", "box");
                     when others => null;
                  end case;
               when others => null;
            end case;

         when A_Definition =>              -- Asis.Definitions
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Definition_Kinds'Image
                     (Asis.Elements.Definition_Kind (Element)))));

            case Asis.Elements.Definition_Kind (Element) is

               when A_Type_Definition =>
                  Tmp :=
                    DOM.Core.Nodes.Append_Child
                    (State.Current,
                     DOM.Core.Documents.Create_Element
                       (State.Document,
                        To_Tag_Name
                          (Type_Kinds'Image
                             (Asis.Elements.Type_Kind (Element)))));
                  case Asis.Elements.Type_Kind (Element) is
                     when An_Access_Type_Definition =>
                        DOM.Core.Elements.Set_Attribute
                          (Tmp,
                           "kind",
                           To_Tag_Name
                             (Access_Type_Kinds'Image
                                (Asis.Elements.Access_Type_Kind (Element))));
                    when A_Derived_Type_Definition |
                      A_Derived_Record_Extension_Definition |
                      A_Record_Type_Definition |
                      A_Tagged_Record_Type_Definition =>
                       Add_Trait (Asis.Elements.Trait_Kind (Element),
                                  State.Current);
                     when A_Root_Type_Definition =>
                        DOM.Core.Elements.Set_Attribute
                          (Tmp,
                           "kind",
                           To_Tag_Name
                             (Root_Type_Kinds'Image
                                (Asis.Elements.Root_Type_Kind (Element))));
                     when others => null;
                  end case;

               when A_Constraint =>
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "kind",
                     To_Tag_Name
                       (Constraint_Kinds'Image
                          (Asis.Elements.Constraint_Kind (Element))));

               when A_Formal_Type_Definition =>
                  Tmp :=
                    DOM.Core.Nodes.Append_Child
                    (State.Current,
                     DOM.Core.Documents.Create_Element
                       (State.Document,
                        To_Tag_Name
                          (Formal_Type_Kinds'Image
                             (Asis.Elements.Formal_Type_Kind (Element)))));
                  case Asis.Elements.Formal_Type_Kind (Element) is
                     when  A_Formal_Access_Type_Definition =>
                        DOM.Core.Elements.Set_Attribute
                          (Tmp,
                           "kind",
                           To_Tag_Name
                             (Access_Type_Kinds'Image
                                (Asis.Elements.Access_Type_Kind (Element))));
                     when A_Formal_Private_Type_Definition |
                       A_Formal_Tagged_Private_Type_Definition |
                       A_Formal_Derived_Type_Definition =>
                        Add_Trait (Asis.Elements.Trait_Kind (Element),
                                   State.Current);
                     when others => null;
                  end case;

               when A_Discrete_Subtype_Definition |
                 A_Discrete_Range =>
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "kind",
                     To_Tag_Name
                       (Discrete_Range_Kinds'Image
                          (Asis.Elements.Discrete_Range_Kind (Element))));

               when A_Component_Definition |
                 A_Private_Type_Definition |
                 A_Tagged_Private_Type_Definition |
                 A_Private_Extension_Definition =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);

               when others => null;

            end case;

            --  Special support for record component size.
            if Asis.Elements.Definition_Kind (Element) = A_Subtype_Indication
            then
               declare
                  N : DOM.Core.Node := State.Current;
               begin
                  while N /= null
                    and then DOM.Core.Nodes.Node_Name (N)
                    /= "component_definition" loop
                     N := DOM.Core.Nodes.Parent_Node (N);
                  end loop;
--                    if N /= null then
--                       DOM.Core.Elements.Set_Attribute
--                         (N,
--                          "size",
--                          Ada.Strings.Fixed.Trim
--                            (Asis.ASIS_Natural'Image
--                               (Asis.Data_Decomposition.Size (Element)),
--                             Ada.Strings.Both));
--                    end if;
               end;
            end if;

         when An_Expression =>             -- Asis.Expressions
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Expression_Kinds'Image
                     (Asis.Elements.Expression_Kind (Element)))));
           case Asis.Elements.Expression_Kind (Element) is
              when An_Attribute_Reference =>
                 Tmp :=
                   DOM.Core.Nodes.Append_Child
                   (State.Current,
                    DOM.Core.Documents.Create_Text_Node
                      (State.Document,
                       To_Tag_Name
                         (Attribute_Kinds'Image
                            (Asis.Elements.Attribute_Kind (Element)))));
              when An_Identifier |
                An_Operator_Symbol |
                A_Character_Literal |
                An_Enumeration_Literal =>
                 Tmp :=
                   DOM.Core.Nodes.Append_Child
                   (State.Current,
                    DOM.Core.Documents.Create_Text_Node
                      (State.Document,
                       +Asis.Expressions.Name_Image (Element)));
              when An_Integer_Literal |
                A_Real_Literal |
                A_String_Literal =>
                 Tmp :=
                   DOM.Core.Nodes.Append_Child
                   (State.Current,
                    DOM.Core.Documents.Create_Text_Node
                      (State.Document,
                       +Asis.Expressions.Value_Image (Element)));
              when A_Function_Call =>
                 if Asis.Expressions.Is_Prefix_Call (Element) then
                    DOM.Core.Elements.Set_Attribute
                      (State.Current,
                       "prefixed",
                       "true");
                 else
                    DOM.Core.Elements.Set_Attribute
                      (State.Current,
                       "prefixed",
                       "false");
                 end if;
              when others => null;
           end case;

         when An_Association =>            -- Asis.Expressions
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Association_Kinds'Image
                     (Asis.Elements.Association_Kind (Element)))));

         when A_Statement =>               -- Asis.Statements
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Statement_Kinds'Image
                     (Asis.Elements.Statement_Kind (Element)))));

         when A_Path =>                    -- Asis.Statements
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Path_Kinds'Image
                     (Asis.Elements.Path_Kind (Element)))));

         when A_Clause =>                  -- Asis.Clauses
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Clause_Kinds'Image
                     (Asis.Elements.Clause_Kind (Element)))));
            case Asis.Elements.Clause_Kind (Element) is
               when A_Representation_Clause =>
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "representation_kind",
                     To_Tag_Name
                       (Representation_Clause_Kinds'Image
                          (Asis.Elements.Representation_Clause_Kind
                             (Element))));
               when others => null;
            end case;

         when An_Exception_Handler =>
            --  Doesn't seem to be a Statement in spite of indication
            --  in Asis.
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document, "exception_handler"));

         when Not_An_Element =>
            null;

      end case;

   end Pre;

   procedure Post (Element : in Asis.Element;
                   Control : in out Asis.Traverse_Control;
                   State : in out Info) is
   begin
--        Put_Line ("Post (" & Asis.Elements.Element_Kind (Element)'Img & ")");
      State.Current := DOM.Core.Nodes.Parent_Node (State.Current);
   end Post;

end XML_Support;
