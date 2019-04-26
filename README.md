# ASIS2XML #

ASIS is the
[Ada Semantic Interface Specification](http://www.acm.org/sigada/WG/asiswg/).

This program converts a unit's ASIS representation into XML, so as to
make it easier to develop reporting and transformational tools using
(for example) XSLT.

<p>There is no XML Schema as yet. The output's structure is quite
close to that of ASIS, at least in overall terms; for example,
an `A_Defining_Name` element in ASIS is represented as a
`&lt;defining_name/&gt;` element in XML. In turn, ASIS's structure is
that of the Ada RM.

<p>One difference is that, for the kinds of element that have
visible/private parts (normal and generic packages, tasks and
protected types) the visible and private parts are enclosed in
`&lt;visible_part/&gt;` and `&lt;private_part/&gt;` elements
respectively.

## Example ##

<p>The source unit `demo.adb`,

    procedure Demo (X : in out Integer) is
    begin
       X := X + 1;
    end Demo;

results, after using _tidy_, in

    <asis>
      <compilation_unit file="/Users/simon/asis2xml/demo.adb"
      unit="Demo">
        <context_clauses></context_clauses>
        <unit_declaration>
          <procedure_body_declaration>
            <defining_identifier>Demo</defining_identifier>
            <parameter_specification mode="inout">
              <defining_identifier>X</defining_identifier>
              <identifier>Integer</identifier>
            </parameter_specification>
            <assignment_statement>
              <identifier>X</identifier>
              <function_call prefixed="false">
                <parameter_association>
                  <identifier>X</identifier>
                </parameter_association>
                <operator_symbol>"+"</operator_symbol>
                <parameter_association>
                  <integer_literal>1</integer_literal>
                </parameter_association>
              </function_call>
            </assignment_statement>
          </procedure_body_declaration>
        </unit_declaration>
        <compilation_pragmas></compilation_pragmas>
      </compilation_unit>
    </asis>

This project was originally hosted on SourceForge as part
of [ASIS for GNAT](https://sourceforge.net/projects/gnat-asis/), and
releases up to 20130413 can be found there.

Later, it moved
to [ASIS2XML](https://sourceforge.net/projects/asis2xml/), also on
SourceForge, and releases up to 20190426 can be found there.

## Copyright ##

This work is derived from the Node\_Trav component of Display\_Source,
which used to be distributed as a part of the ASIS implementation for
GNAT and is Copyright (c) 1995-1999, Free Software Foundation, Inc.

The original work in the program is Copyright (c) Simon Wright
`<simon@pushface.org>`.

## Licensing ##

The work is distributed under the terms of
the [GPL, version 2](http://www.gnu.org/copyleft/gpl.html).

## Prerequisites ##

* GNAT: GPL 2012 or later, or GCC 4.8 or later

* The corresponding GNAT ASIS

* [XML/Ada](https://github.com/AdaCore/xmlada) 1.0 or later

## Use ##

`asis2xml` will accept a tree file (`.adt`), or a directory which is a
GNAT object directory containing a set of `.adt` files. The GNAT ASIS
implementation doesn't understand Project files, so if you have any
sort of complex path setup the way to go is to generate the tree files
using the Project:

    $ gprbuild -Pbuild -c -u -f -gnatct example.ads

creates `example.adt` in your project (`build.gpr`)'s `Object_Dir` -- in the
case of `asis2xml`'s `asis2xml.gpr`, this is `./.build`.

    $ asis2xml .build/example.adt >example.xml

The output is in "packed" XML; to get a more legible view, you can
use [HTML Tidy](http://www.html-tidy.org):

    $ asis2xml .build/example.adt | tidy -xml -i -utf8 >example.tidy.xml

or variations.
