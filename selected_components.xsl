<?xml version="1.0" encoding="utf-8"?>
<!-- Copyright (C) 2019 Simon Wright <simon@pushface.org> -->

<!-- Construct an 'expanded name' (see ARM 4.1.3 (4)) to contain
     selected_components; cf AdaCore's defining_expanded_name. -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="xml" encoding="iso-8859-1" indent="no"/>
  <xsl:strip-space elements="*"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="selected_component">
    <xsl:variable name="sc" select="."/>
    <xsl:variable name="en">
      <xsl:for-each select="descendant::identifier
                            | descendant::operator_symbol
                            | descendant::character_literal">
        <xsl:value-of select="."/>
        <xsl:if test="not(position()=last())">
          <xsl:text>.</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:element name="expanded_name">
      <xsl:value-of select="$en"/>
      <xsl:copy-of select="$sc"/>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
