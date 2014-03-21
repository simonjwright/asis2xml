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
--  Developed from Display_Source, which is distributed as a part of
--  the ASIS implementation for GNAT and is Copyright (c) 1995-1999,
--  Free Software Foundation, Inc.

with Ada;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Characters.Handling;

with Asis; use Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Data_Decomposition;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Iterator;

with DOM.Core.Nodes;

with XML_Support;

procedure ASIS2XML is

   function "+" (Item : String) return Wide_String
     renames Ada.Characters.Handling.To_Wide_String;

   function "+" (Item : Wide_String) return String;
   function "+" (Item : Wide_String) return String is
   begin
      return Ada.Characters.Handling.To_String (Item);
   end "+";

   procedure Traverse_Tree_For_XML is new Asis.Iterator.Traverse_Element
     (XML_Support.Info,
      XML_Support.Pre,
      XML_Support.Post);

   --  Some global variables.
   SS_Context : Asis.Context;
   The_Unit : Asis.Compilation_Unit;
   The_Declaration : Asis.Declaration;
   The_Control : Asis.Traverse_Control := Asis.Continue;
   Source_File_Command_Index : Positive := 1;
   Source_Is_Tree_File : Boolean := False;

   XI : XML_Support.Info;
   Impl : DOM.Core.DOM_Implementation;
   Doc : DOM.Core.Document;

begin

   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line
        ("usage: " &
          Ada.Command_Line.Command_Name & " Unit[.ads|.adb]");
      raise Numeric_Error;
   end if;

   declare
      File_Name : String
        renames Ada.Command_Line.Argument (Source_File_Command_Index);
      Extension : constant String
        := File_Name (File_Name'Last - 3 .. File_Name'Last);
      Unit_Name : String := File_Name (File_Name'First .. File_Name'Last - 4);
   begin

      if Extension = ".adt" then
         Source_Is_Tree_File := True;
      elsif Extension /= ".ads" then
         Put_Line ("must be a .ads or .adt file.");
         return;
      end if;

      Asis.Implementation.Initialize;

      if Source_Is_Tree_File then
         Asis.Ada_Environments.Associate
           (The_Context => SS_Context,
            Name => "SS_Context",
            Parameters  => "-SN -C1 " & (+File_Name));
      else
         Asis.Ada_Environments.Associate
           (The_Context => SS_Context,
            Name => "SS_Context",
            Parameters  => "-FS");
      end if;

      Asis.Ada_Environments.Open (SS_Context);

      for C in Unit_Name'Range loop
         if Unit_Name (C) = '-' then
            Unit_Name (C) := '.';
         end if;
      end loop;

      The_Unit :=
        Asis.Compilation_Units.Library_Unit_Declaration
        (+Unit_Name, SS_Context);

      if (Asis.Compilation_Units.Is_Nil (The_Unit)) then

         --  Try for a body ..

         The_Unit :=
           Asis.Compilation_Units.Compilation_Unit_Body
           (+Unit_Name, SS_Context);

         if (Asis.Compilation_Units.Is_Nil (The_Unit)) then
            Put_Line ("Unit " & File_Name & " is Nil...");
            Asis.Ada_Environments.Close (SS_Context);
            raise Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit;
         end if;

      end if;

   end;

   The_Declaration := Asis.Elements.Unit_Declaration (The_Unit);

   The_Control := Asis.Continue;

   Doc  := DOM.Core.Create_Document (Impl);
   XML_Support.Initialize (XI,
                           The_Declaration,
                           Doc);

   Traverse_Tree_For_XML (The_Declaration, The_Control, XI);

   XML_Support.Finalize (XI);

   Asis.Ada_Environments.Close (SS_Context);
   Asis.Ada_Environments.Dissociate (SS_Context);
   Asis.Implementation.Finalize ("");

   DOM.Core.Nodes.Print (Doc,
                         Print_Comments => True,
                         Print_XML_PI => True);

   --  delete any generated files
   declare
      To_Erase : String :=
        Ada.Command_Line.Argument (Source_File_Command_Index);
      File : File_Type;
   begin
      if To_Erase (To_Erase'Last - 3 .. To_Erase'Last - 1) = ".ad" then

         if not Source_Is_Tree_File then

            Open (File,
                  Out_File,
                  To_Erase (To_Erase'First .. To_Erase'Last - 1) & "t");
            Delete (File);

            begin
               Open (File,
                     Out_File,
                     To_Erase (To_Erase'First .. To_Erase'Last - 3) & "ali");
               Delete (File);
            exception
               when others => null;
            end;

            begin
               Open (File,
                     Out_File,
                     To_Erase (To_Erase'First .. To_Erase'Last - 3) & "o");
               Delete (File);
            exception
               when others => null;
            end;

         end if;

      end if;
   end;

exception

   when Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit =>
      Put_Line ("The file "
                  & Ada.Command_Line.Argument (Source_File_Command_Index) &
                  " does not contain any Ada Unit.");
      New_Line;
      Put_Line
        ("usage: " & Ada.Command_Line.Command_Name & " Unit.ads");
      raise;

   when Asis.Exceptions.ASIS_Failed |
        Asis.Exceptions.ASIS_Inappropriate_Element |
        Asis.Exceptions.ASIS_Inappropriate_Context =>
         Put_Line (Ada.Characters.Handling.To_String
                     (Asis.Implementation.Diagnosis));
      raise;

   when E : others =>
      Put_Line ("exception received : " &
                 Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Characters.Handling.To_String
                  (Asis.Implementation.Diagnosis));
      raise;

end ASIS2XML;
