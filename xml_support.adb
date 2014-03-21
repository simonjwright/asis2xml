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

            case Asis.Elements.Declaration_Kind (Element) is
               when A_Private_Type_Declaration |
                 A_Private_Extension_Declaration |
                 A_Variable_Declaration |
                 A_Constant_Declaration |
                 A_Deferred_Constant_Declaration |
                 A_Discriminant_Specification |
                 A_Loop_Parameter_Specification |
                 A_Procedure_Declaration |
                 A_Function_Declaration |
                 A_Parameter_Specification =>
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "kind",
                     To_Tag_Name
                       (Trait_Kinds'Image
                          (Asis.Elements.Trait_Kind (Element))));
               when A_Formal_Function_Declaration |
                    A_Formal_Procedure_Declaration =>
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "kind",
                     To_Tag_Name
                       (Subprogram_Default_Kinds'Image
                          (Asis.Elements.Default_Kind (Element))));
               when others => null;
            end case;

            case Asis.Elements.Declaration_Kind (Element) is
               when A_Parameter_Specification |
                 A_Formal_Object_Declaration =>
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "mode",
                     To_Tag_Name
                       (Mode_Kinds'Image
                          (Asis.Elements.Mode_Kind (Element))));
               when others =>
                  null;
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
                        DOM.Core.Elements.Set_Attribute
                          (Tmp,
                           "kind",
                           To_Tag_Name
                             (Trait_Kinds'Image
                                (Asis.Elements.Trait_Kind (Element))));
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
                        DOM.Core.Elements.Set_Attribute
                          (Tmp,
                           "kind",
                           To_Tag_Name
                             (Trait_Kinds'Image
                                (Asis.Elements.Trait_Kind (Element))));
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
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "kind",
                     To_Tag_Name
                       (Trait_Kinds'Image
                          (Asis.Elements.Trait_Kind (Element))));

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
                  if N /= null then
                     DOM.Core.Elements.Set_Attribute
                       (N,
                        "size",
                        Ada.Strings.Fixed.Trim
                          (Asis.ASIS_Natural'Image
                             (Asis.Data_Decomposition.Size (Element)),
                           Ada.Strings.Both));
                  end if;
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
              when others =>
                 null;
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
               when others =>
                  null;
            end case;

         when others =>
            State.Current :=
            DOM.Core.Nodes.Append_Child
            (State.Current,
             DOM.Core.Documents.Create_Element
               (State.Document,
                To_Tag_Name
                  (Element_Kinds'Image
                     (Asis.Elements.Element_Kind (Element)))));


      end case;

   end Pre;

   procedure Post (Element : in Asis.Element;
                   Control : in out Asis.Traverse_Control;
                   State : in out Info) is
   begin
      State.Current := DOM.Core.Nodes.Parent_Node (State.Current);
   end Post;

end XML_Support;
