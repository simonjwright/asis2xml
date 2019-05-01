--  Copyright 2004-2014, 2018-2019 Simon Wright <simon@pushface.org>

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
--  Developed from the Node_Trav component of Display_Source, which is
--  distributed as a part of the ASIS implementation for GNAT and is
--  Copyright (c) 1995-1999, Free Software Foundation, Inc.

with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);

with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Expressions;
with Asis.Iterator;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

package body XML_Support is

   ------------------------------
   --  Local subprogram specs  --
   ------------------------------

   --  Wide_String => String conversion.
   function "+" (Item : Wide_String) return String;

   --  Traits (see Asis spec) are an additional classification
   --  mechanism that lets Asis use fewer high-level elements. Not
   --  every A_Definition element supports traits, and not every trait
   --  is applicable in all circumstances. This code assumes that the
   --  Asis tree is well-formed, so if a trait is present it is legal
   --  and we just add the corresponding attribute to the DOM tree.
   procedure Add_Trait (K : Asis.Trait_Kinds; To : DOM.Core.Node);

   --  Conversion of Asis enumeration literals, typically A_Thing or
   --  An_Object, to our canonical form (leading article removed, all
   --  lower case).
   function To_Tag_Name (Image : String) return String;

   --  Tree traversal
   procedure Pre (Element : in     Asis.Element;
                  Control : in out Asis.Traverse_Control;
                  State   : in out Info);
   procedure Post (Element : in     Asis.Element;
                   Control : in out Asis.Traverse_Control;
                   State   : in out Info);
   procedure Traverse_Tree_For_XML is new Asis.Iterator.Traverse_Element
     (State_Information => Info,
      Pre_Operation     => Pre,
      Post_Operation    => Post);

   --  Tree traversal for expanded_name processing. On first
   --  encountering a Selected_Name, this iterator is invoked to
   --  concatenate (with periods) the name-like components of the
   --  Selected_Name and its children.
   procedure Pre_For_Expanded_Name (Element : in     Asis.Element;
                                    Control : in out Asis.Traverse_Control;
                                    State   : in out Unbounded_String);
   procedure Post_For_Expanded_Name (Element : in     Asis.Element;
                                     Control : in out Asis.Traverse_Control;
                                     State   : in out Unbounded_String)
     is null;
   procedure Traverse_Tree_For_Expanded_Name
     is new Asis.Iterator.Traverse_Element
       (State_Information => Unbounded_String,
        Pre_Operation     => Pre_For_Expanded_Name,
        Post_Operation    => Post_For_Expanded_Name);


   ---------------------------------------
   --  Spec subprogram implementations  --
   ---------------------------------------

   procedure Add_Compilation_Unit (The_Unit : Asis.Compilation_Unit;
                                   To       : in out Info) is
      The_Control : Asis.Traverse_Control := Asis.Continue;
      Starting : constant DOM.Core.Node := To.Current;
      --  Remember the starting node (will be <asis/>)
      Unit : DOM.Core.Node;
   begin

      --  Add the <compilation_unit/> element.
      Unit := DOM.Core.Nodes.Append_Child
        (Starting,
         DOM.Core.Documents.Create_Element (To.Document, "compilation_unit"));
      DOM.Core.Elements.Set_Attribute
        (Unit,
         "unit",
         +Asis.Compilation_Units.Unit_Full_Name (The_Unit));
      DOM.Core.Elements.Set_Attribute
        (Unit,
         "file",
         +Asis.Compilation_Units.Text_Name (The_Unit));

      --  Add the context clauses (if any).
      declare
         Context_Clauses : constant Asis.Context_Clause_List
           := Asis.Elements.Context_Clause_Elements
             (Compilation_Unit => The_Unit,
              Include_Pragmas => True);
         Context : constant DOM.Core.Node
           := DOM.Core.Nodes.Append_Child
             (Unit,
              DOM.Core.Documents.Create_Element (To.Document,
                                                 "context_clauses"));
      begin
         for C in Context_Clauses'Range loop
            To.Current := Context;
            Traverse_Tree_For_XML (Element => Context_Clauses (C),
                                   Control => The_Control,
                                   State   => To);
         end loop;
      end;

      --  Add the declaration.
      To.Current := DOM.Core.Nodes.Append_Child
        (Unit,
         DOM.Core.Documents.Create_Element (To.Document, "unit_declaration"));
      Traverse_Tree_For_XML
        (Element => Asis.Elements.Unit_Declaration (The_Unit),
         Control => The_Control,
         State   => To);

      --  Add the compilation pragmas (if any).
      declare
         Compilation_Pragmas : constant Asis.Pragma_Element_List
           := Asis.Elements.Compilation_Pragmas (The_Unit);
         Pragmas : constant DOM.Core.Node
           := DOM.Core.Nodes.Append_Child
             (Unit,
              DOM.Core.Documents.Create_Element (To.Document,
                                                 "compilation_pragmas"));
      begin
         for C in Compilation_Pragmas'Range loop
            To.Current := Pragmas;
            Traverse_Tree_For_XML (Element => Compilation_Pragmas (C),
                                   Control => The_Control,
                                   State   => To);
         end loop;
      end;

      --  Restore the starting node.
      To.Current := Starting;

   end Add_Compilation_Unit;


   procedure Initialize (XI : in out Info;
                         Document : DOM.Core.Node) is
   begin
      XI.Document := Document;
      XI.Current := DOM.Core.Nodes.Append_Child
        (XI.Document,
         DOM.Core.Documents.Create_Element (XI.Document, "asis"));
   end Initialize;


   procedure Finalize (XI : in out Info) is
      pragma Unreferenced (XI);
   begin
      null;
   end Finalize;


   ----------------------------------------
   --  Local subprogram implementations  --
   ----------------------------------------

   function "+" (Item : Wide_String) return String is
   begin
      return Ada.Characters.Handling.To_String (Item);
   end "+";


   procedure Add_Trait (K : Asis.Trait_Kinds; To : DOM.Core.Node) is
   begin
      case K is
         when Asis.Not_A_Trait | Asis.An_Ordinary_Trait => null;
         when Asis.An_Aliased_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "aliased", "true");
         when Asis.An_Access_Definition_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "access", "true");
         when Asis.A_Null_Exclusion_Trait =>
            DOM.Core.Elements.Set_Attribute (To, "not_null", "true");
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


   procedure Pre (Element : in     Asis.Element;
                  Control : in out Asis.Traverse_Control;
                  State   : in out Info) is
      pragma Unreferenced (Control);

      --  If this is the kind of element that has visible/private
      --  parts, we find and traverse the visible and private parts
      --  separately inside <visible_part/> and <private_part/>
      --  respectively.

      procedure Handle_Content_With_Visibility
        (Content            : Asis.Declarative_Item_List;
         Visibility_Element : String);
      procedure Handle_Content_With_Visibility
        (Content            : Asis.Declarative_Item_List;
         Visibility_Element : String) is
         Local_Control : Asis.Traverse_Control := Asis.Continue;
      begin
         State.Current := DOM.Core.Nodes.Append_Child
           (State.Current,
            DOM.Core.Documents.Create_Element
              (State.Document, Visibility_Element));
         for J in Content'Range loop
            Traverse_Tree_For_XML (Element => Content (J),
                                   Control => Local_Control,
                                   State   => State);
         end loop;
         State.Current := DOM.Core.Nodes.Parent_Node (State.Current);
      end Handle_Content_With_Visibility;

      Tmp : DOM.Core.Node;
      use Asis;

   begin

      --  Put_Line (Standard_Error,
      --            "Pre (" & Asis.Elements.Element_Kind (Element)'Img & ")");

      case Asis.Elements.Element_Kind (Element) is

         when A_Pragma =>                  -- Asis.Elements
            State.Current :=
              DOM.Core.Nodes.Append_Child
                (State.Current,
                 DOM.Core.Documents.Create_Element
                   (State.Document,
                    "pragma"));
            declare
               Kind : constant String :=
                 To_Tag_Name
                   (Pragma_Kinds'Image
                      (Asis.Elements.Pragma_Kind (Element)));
               Last : constant Integer
                 := Kind'Last - String'("_pragma")'Length;
            begin
               DOM.Core.Elements.Set_Attribute
                 (State.Current,
                  "kind",
                  Kind (Kind'First .. Last));
               Tmp :=
                 DOM.Core.Nodes.Append_Child
                   (State.Current,
                    DOM.Core.Documents.Create_Element
                      (State.Document,
                       "identifier"));
               Tmp := DOM.Core.Nodes.Append_Child
                 (Tmp,
                  DOM.Core.Documents.Create_Text_Node
                    (State.Document,
                     +Asis.Elements.Pragma_Name_Image (Element)));
            end;

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
                  --  XXX what sense of 'should'?
                  case Asis.Elements.Default_Kind (Element) is
                     when A_Name_Default =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "default", "name");
                     when A_Box_Default =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "default", "box");
                     when A_Null_Default =>
                        DOM.Core.Elements.Set_Attribute
                          (State.Current, "default", "null"); -- 2005
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
                        Add_Trait (Asis.Elements.Trait_Kind (Element),
                                   Tmp);
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
                     when An_Interface_Type_Definition =>
                        --  2005: Interface_Kinds
                        DOM.Core.Elements.Set_Attribute
                          (Tmp,
                           "kind",
                           To_Tag_Name
                             (Interface_Kinds'Image
                                (Asis.Elements.Interface_Kind (Element))));
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
                     when A_Formal_Interface_Type_Definition =>
                        --  2005: Interface_Kinds
                        DOM.Core.Elements.Set_Attribute
                          (Tmp,
                           "kind",
                           To_Tag_Name
                             (Interface_Kinds'Image
                                (Asis.Elements.Interface_Kind (Element))));
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

               when A_Subtype_Indication |
                 A_Component_Definition |
                 A_Private_Type_Definition |
                 A_Tagged_Private_Type_Definition |
                 A_Private_Extension_Definition =>
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);

               when An_Access_Definition =>
                  --  2005: Access_Definition_Kinds
                  DOM.Core.Elements.Set_Attribute
                    (State.Current,
                     "kind",
                     To_Tag_Name
                       (Access_Definition_Kinds'Image
                          (Asis.Elements.Access_Definition_Kind (Element))));
                  Add_Trait (Asis.Elements.Trait_Kind (Element),
                             State.Current);

               when others => null;

            end case;

         when An_Expression =>             -- Asis.Expressions

            --  Special handing for <expanded_name/>.
            if Asis.Elements.Expression_Kind (Element) = A_Selected_Component
              and then not State.Processing_Expanded_Name
            then
               --  Avoid unbounded recursion.
               State.Processing_Expanded_Name := True;
               declare
                  Current : constant DOM.Core.Node := State.Current;
                  Local_Control : Asis.Traverse_Control := Asis.Continue;
                  Expanded_Name : Unbounded_String;
               begin
                  --  First, stop current interation delving into children.
                  Control       := Asis.Abandon_Children;
                  --  Add an <expanded_name/> element.
                  State.Current := DOM.Core.Nodes.Append_Child
                    (State.Current,
                     DOM.Core.Documents.Create_Element
                       (State.Document, "expanded_name"));
                  --  Build the expanded name.
                  Traverse_Tree_For_Expanded_Name (Element => Element,
                                                   Control => Local_Control,
                                                   State   => Expanded_Name);
                  --  Add the expanded name as a text child of the
                  --  <expanded_name/> element.
                  Tmp := DOM.Core.Nodes.Append_Child
                    (State.Current,
                     DOM.Core.Documents.Create_Text_Node
                       (State.Document,
                        To_String (Expanded_Name)));
                  --  Process the children, output under the
                  --  <expanded_name/> element.
                  Traverse_Tree_For_XML (Element => Element,
                                         Control => Local_Control,
                                         State   => State);
                  State.Current := Current;
               end;
               State.Processing_Expanded_Name := False;

            --  Normal processing, possibly as the internal contents
            --  of an <expanded_name/>.
            else
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
                     declare
                        Kind : constant String :=
                          To_Tag_Name
                            (Attribute_Kinds'Image
                               (Asis.Elements.Attribute_Kind (Element)));
                        Last : constant Integer
                          := Kind'Last - String'("_attribute")'Length;
                     begin
                        DOM.Core.Elements.Set_Attribute
                          (State.Current,
                           "kind",
                           Kind (Kind'First .. Last));
                     end;
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
            end if;

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

      --  Now handle the cases where we need to handle visible/private
      --  parts. In these cases we prevent the incoming iteration from
      --  descending further and do the iteration via a recursive
      --  traversal, first inserting a <visible_part/> or
      --  <private_part/> element as appropriate.

      --  For (generic) packages, the relevant element is the
      --  declaration.
      case Asis.Elements.Declaration_Kind (Element) is
         when
           A_Generic_Package_Declaration |
           A_Package_Declaration
           =>
            Control := Asis.Abandon_Children;
            declare
               Local_Control : Asis.Traverse_Control := Asis.Continue;
               Names : constant Asis.Defining_Name_List -- should have length 1
                 := Asis.Declarations.Names (Element);
            begin
               if Names'Length /= 1 then
                  raise Constraint_Error
                    with "package has A_Defining_Name_List with length /= 1";
               end if;
               Traverse_Tree_For_XML (Element => Names (Names'First),
                                      Control => Local_Control,
                                      State   => State);
            end;
            Handle_Content_With_Visibility
              (Content            =>
                 Asis.Declarations.Visible_Part_Declarative_Items
                   (Declaration     => Element,
                    Include_Pragmas => True),
               Visibility_Element => "visible_part");
            Handle_Content_With_Visibility
              (Content            =>
                 Asis.Declarations.Private_Part_Declarative_Items
                   (Declaration     => Element,
                    Include_Pragmas => True),
               Visibility_Element => "private_part");

         when others =>
            null;
      end case;

      --  For protected and task types and objects, the relevant
      --  elements are definitions.
      case Asis.Elements.Definition_Kind (Element) is
         when
           A_Protected_Definition |
           A_Task_Definition
           =>
            Control := Asis.Abandon_Children;
            Handle_Content_With_Visibility
              (Content => Asis.Definitions.Visible_Part_Items
                 (Definition      => Element,
                  Include_Pragmas => True),
               Visibility_Element => "visible_part");
            Handle_Content_With_Visibility
              (Content => Asis.Definitions.Private_Part_Items
                 (Definition      => Element,
                  Include_Pragmas => True),
               Visibility_Element => "private_part");

         when others =>
            null;
      end case;

   end Pre;


   procedure Post (Element : in     Asis.Element;
                   Control : in out Asis.Traverse_Control;
                   State   : in out Info) is
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
   begin
      --  Put_Line (Standard_Error,
      --            "Post (" & Asis.Elements.Element_Kind (Element)'Img & ")");
      State.Current := DOM.Core.Nodes.Parent_Node (State.Current);
   end Post;


   procedure Pre_For_Expanded_Name (Element : in     Asis.Element;
                                    Control : in out Asis.Traverse_Control;
                                    State   : in out Unbounded_String)
   is
      pragma Unreferenced (Control);
   begin
      case Asis.Elements.Expression_Kind (Element) is
         when Asis.An_Identifier |
           Asis.An_Operator_Symbol |
           Asis.A_Character_Literal =>
            if State /= Null_Unbounded_String then
               Append (State, ".");
            end if;
            Append (State, +Asis.Expressions.Name_Image (Element));
         when others =>
            null;
      end case;
   end Pre_For_Expanded_Name;


   function To_Tag_Name (Image : String) return String is
      Result : constant String := Ada.Characters.Handling.To_Lower (Image);
   begin
      if Result (1 .. 2) = "a_" then
         return Result (3 .. Result'Last);
      elsif Result (1 .. 3) = "an_" then
         return Result (4 .. Result'Last);
      else
         return Result;
      end if;
   end To_Tag_Name;


end XML_Support;
