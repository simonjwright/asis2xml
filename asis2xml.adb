--  Copyright 2004-2006 Simon Wright <simon@pushface.org>

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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with Asis; use Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Exceptions;
with Asis.Implementation;

with DOM.Core.Nodes;

with XML_Support;

procedure ASIS2XML is

   function "+" (Item : String) return Wide_String
     renames Ada.Characters.Handling.To_Wide_String;

   procedure Usage;
   procedure Usage is
   begin
      Put_Line (Standard_Error,
                "usage: " &
                  Ada.Command_Line.Command_Name
                  & " [flags] directory|unit.adt");
      Put_Line (Standard_Error, "flags:");
      Put_Line (Standard_Error, "-h          produce this output");
   end Usage;

   --  Some global variables.
   SS_Context : Asis.Context;

   XI : XML_Support.Info;
   Impl : DOM.Core.DOM_Implementation;
   Doc : DOM.Core.Document;

begin

   Asis.Implementation.Initialize ("-asis05");

   begin
      loop
         case GNAT.Command_Line.Getopt ("h") is
            when ASCII.NUL => exit;
            when 'h' =>
               Usage;
               return;
            when others => null;
               --  can't actually happen, raises Invalid_Switch
         end case;
      end loop;
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Usage;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end;

   declare
      File_Name : constant String := GNAT.Command_Line.Get_Argument;
   begin
      if File_Name'Length = 0 then
         Usage;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      elsif GNAT.OS_Lib.Is_Directory (File_Name) then
         Asis.Ada_Environments.Associate
           (The_Context => SS_Context,
            Name => "SS_Context",
            Parameters  => "-SN -CA -FT -T" & (+File_Name));
      elsif GNAT.Directory_Operations.File_Extension (File_Name) = ".adt" then
         Asis.Ada_Environments.Associate
           (The_Context => SS_Context,
            Name => "SS_Context",
            Parameters  => "-SN -C1 " & (+File_Name));
      else
         Usage;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end;

   Asis.Ada_Environments.Open (SS_Context);

   Doc  := DOM.Core.Create_Document (Impl);
   XML_Support.Initialize (XI, Doc);

   declare
      Next_Unit : Asis.Compilation_Unit;
      All_Units : constant Asis.Compilation_Unit_List
        := Asis.Compilation_Units.Compilation_Units (SS_Context);
   begin
      for I in All_Units'Range loop
         Next_Unit := All_Units (I);
         if Asis.Compilation_Units.Unit_Origin (Next_Unit)
           = Asis.An_Application_Unit then
            XML_Support.Add_Compilation_Unit (The_Unit => Next_Unit,
                                              To => XI);
         end if;
      end loop;
   end;

   XML_Support.Finalize (XI);

   Asis.Ada_Environments.Close (SS_Context);
   Asis.Ada_Environments.Dissociate (SS_Context);
   Asis.Implementation.Finalize ("");

   DOM.Core.Nodes.Print (Doc,
                         Print_Comments => True,
                         Print_XML_PI => True,
                         EOL_Sequence => "");

exception

   when Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit =>
      Put_Line (Standard_Error,
                "The input does not contain any Ada unit.");
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Asis.Exceptions.ASIS_Failed |
        Asis.Exceptions.ASIS_Inappropriate_Element |
        Asis.Exceptions.ASIS_Inappropriate_Context =>
      Put_Line (Standard_Error,
                Ada.Characters.Handling.To_String
                  (Asis.Implementation.Diagnosis));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Put_Line (Standard_Error,
                "exception received : " &
                 Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end ASIS2XML;
