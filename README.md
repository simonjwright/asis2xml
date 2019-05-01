# ASIS2XML #

ASIS is the
[Ada Semantic Interface Specification](http://www.acm.org/sigada/WG/asiswg/).

This program converts a unit's ASIS representation into XML, so as to make it easier to develop reporting and transformational tools using (for example) XSLT.

Not every ASIS feature is supported, and in particular

  * What you get corresponds to a straightforward navigation through the tree, there's no cross-linking.
  * There's no attempt to relate the structure to the source text.
  * Comments aren't preserved.

There is no XML Schema as yet. The output's structure is quite close to that of ASIS, at least in overall terms; for example, an `A_Defining_Name` element in ASIS is represented as a `<defining_name/>` element in XML. This is hardly surprising since the default strategy, faced with an ASIS Element, is to translate it to an XML element with the indicated substitution!

In turn, ASIS's structure is largely that of the Ada RM.

## Example ##

The source unit `demo.adb`,
```ada
procedure Demo (X : in out Integer) is
begin
   X := X + 1;
end Demo;
```
results, after using _tidy_, in
```
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
```
This project was originally hosted on SourceForge as part of [ASIS for GNAT](https://sourceforge.net/projects/gnat-asis/), and releases up to 20130413 can be found there.

Later, it moved to [ASIS2XML](https://sourceforge.net/projects/asis2xml/), also on SourceForge, and releases up to 20190426 can be found there.

## Copyright ##

This work is derived from the Node\_Trav component of Display\_Source, which used to be distributed as a part of the ASIS implementation for GNAT and is Copyright (c) 1995-1999, Free Software Foundation, Inc.

The original work in the program is Copyright (c) Simon Wright `<simon@pushface.org>`.

## Licensing ##

The work is distributed under the terms of the [GPL, version 2](http://www.gnu.org/copyleft/gpl.html).

## Prerequisites ##

* GNAT: GPL 2012 or later, or GCC 4.8 or later

* The corresponding GNAT ASIS

* [XML/Ada](https://github.com/AdaCore/xmlada) 1.0 or later

## Use ##

`asis2xml` will accept a tree file (`.adt`), or a directory which is a GNAT object directory containing a set of `.adt` files. The GNAT ASIS implementation doesn't understand Project files, so if you have any sort of complex path setup the way to go is to generate the tree files using the Project:
```sh
$ gprbuild -Pbuild -c -u -f -gnatct example.ads
```
creates `example.adt` in your project (`build.gpr`)'s `Object_Dir` -- in the case of `asis2xml`'s `asis2xml.gpr`, this is `./.build`.
```sh
$ asis2xml .build/example.adt >example.xml
```
The output is in "packed" XML; to get a more legible view, you can
use [HTML Tidy](http://www.html-tidy.org):
```sh
$ asis2xml .build/example.adt | tidy -xml -i -utf8 >example.tidy.xml
```
or variations.

## Differences from GNAT ASIS structure ##

If you want to perform analysis on the generated XML, the best way (absent a schema) is to write sample code and see what _asis2xml_ makes of it. That said,

### Attributes ###

Extensive use is made of attributes (for example, `mode="inout"` above).

### Visible/private parts ###

For the kinds of element that have visible/private parts (normal and generic packages, tasks and protected types) the visible and private parts are enclosed in `<visible_part/>` and `<private_part/>` elements respectively.

### Pragmas ###

Pragmas are represented more naturally:
```xml
<pragma kind="storage_size">
  <identifier>Storage_Size</identifier>
  <pragma_argument_association>
    <integer_literal>2048</integer_literal>
  </pragma_argument_association>
</pragma>
```

### Compound identifiers ###

Compound identifiers are hard to deal with: `with
Ada.Characters.Handling;` becomes, in the default ASIS structure,
```xml
<with_clause>
  <selected_component>
    <selected_component>
      <identifier>Ada</identifier>
      <identifier>Characters</identifier>
    </selected_component>
    <identifier>Handling</identifier>
  </selected_component>
</with_clause>
```
ASIS2XML modifies this by inserting an `<expanded_name/>` element:
```xml
<with_clause>
  <expanded_name>Ada.Characters.Handling
  <selected_component>
    <selected_component>
      <identifier>Ada</identifier>
      <identifier>Characters</identifier>
    </selected_component>
    <identifier>Handling</identifier>
  </selected_component></expanded_name>
</with_clause>
```
This is similar to the GNAT ASIS `<defining_expanded_name/>`, used for
compound program unit names.

Note that both these structures require changes to the way you would normally expect to process the XML:

  * `<expanded_name/>` appears in the places where, if unqualified, you'd expect to find an `<identifier/>`, an `<operator_symbol/>`, or a `<character_literal/>.

  * `<defining_expanded_name/>` appears in the places where, if unqualified, you'd expect to find a `<defining_identifier/>`.

In either case, the XML contains further child elements, so you need to extract the text of just this node:
```xsl
<xsl:value-of select="text()"/>
```
