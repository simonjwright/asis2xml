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
--  Developed from the Node_Trav component of Display_Source, which is
--  distributed as a part of the ASIS implementation for GNAT and is
--  Copyright (c) 1995-1999, Free Software Foundation, Inc.

with Asis;
with DOM.Core;

package XML_Support is

   type Info is private;

   procedure Initialize (XI : in out Info;
                         Document : DOM.Core.Node);

   procedure Add_Compilation_Unit (The_Unit : Asis.Compilation_Unit;
                                   To : in out Info);

   procedure Finalize (XI : in out Info);

private

   type Info is record
      Document : DOM.Core.Document;
      Current : DOM.Core.Node;
   end record;

end XML_Support;
