<?xml version="1.0" encoding="UTF-8"?>

<!--
 ! iso4217ledger.xsl - Transform ISO 4217 Table A.1 to ledger commodities
 !
 ! The current currency & funds code list is found at:
 ! http://www.currency-iso.org/en/home/tables/table-a1.html
 ! -->

<!--
 ! Copyright (c) 2014 Alexis Hildebrandt
 !
 ! Permission is hereby granted, free of charge, to any person obtaining a copy
 ! of this software and associated documentation files (the "Software"), to deal
 ! in the Software without restriction, including without limitation the rights
 ! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 ! copies of the Software, and to permit persons to whom the Software is
 ! furnished to do so, subject to the following conditions:
 ! 
 ! The above copyright notice and this permission notice shall be included in
 ! all copies or substantial portions of the Software.
 !
 ! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 ! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 ! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 ! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 ! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 ! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 ! SOFTWARE.
 ! -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  >
  
  <!--
   ! Set the value of this variable to your preferred decimal separator.
   !  For European countries this is likely to be the comma ','.
   ! -->
  <xsl:variable name="decimal_separator">
    <xsl:text>,</xsl:text>
    <!--
    <xsl:text>.</xsl:text>
    -->
  </xsl:variable>


  <!--
   !  Ensure that plain text will be written,
   !  and all whitespace from the XML source is stripped.
   ! -->
  <xsl:output method="text"/>
  <xsl:template match="text()" />

  <!--
   ! Add comment that the file was generated.
   ! -->
  <xsl:template match="/">
    <xsl:text>; Ledger commodity declarations
; Generated from ISO 4217 Table A.1 XML (</xsl:text>
    <xsl:value-of select="ISO_4217/@Pblshd"/>
<xsl:text>) using iso4217ledger.xsl

</xsl:text>
    <xsl:apply-templates />
  </xsl:template>

  <!--
   !  Create ledger entry for the corresponding commodity
   ! -->
  <xsl:template match="CcyNtry">
    <xsl:variable name="ccy">
      <xsl:choose>
        <xsl:when test="Ccy">
          <xsl:value-of select="Ccy"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>¤</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:text>commodity </xsl:text>
    <xsl:value-of select="$ccy"/>

    <xsl:text>
  note </xsl:text>
    <xsl:value-of select="CcyNm"/>
    <xsl:text> - </xsl:text>
    <xsl:value-of select="normalize-space(CtryNm)"/>
    <xsl:if test="CcyNbr">
      <xsl:text> (</xsl:text>
      <xsl:value-of select="CcyNbr"/>
      <xsl:text>)</xsl:text>
    </xsl:if>

    <xsl:text>
  format </xsl:text>
    <xsl:value-of select="$ccy"/><xsl:text> </xsl:text>
    <xsl:text>0000</xsl:text>
    <xsl:choose>
      <xsl:when test="CcyMnrUnts > 0">
        <xsl:value-of select="$decimal_separator"/>
        <xsl:call-template name="zero">
          <xsl:with-param name="count" select="CcyMnrUnts"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="CcyMnrUnts = 'N.A.'">
        <xsl:value-of select="$decimal_separator"/>
        <xsl:call-template name="zero">
          <xsl:with-param name="count" select="3"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text>
  nomarket

</xsl:text>
  </xsl:template>


  <!--
   ! Recursive template to generate 0s 
   ! -->
  <xsl:template name="zero">
    <xsl:param name="count" select="0"/>
    <xsl:if test="$count > 0">
      <xsl:text>0</xsl:text>
      <xsl:call-template name="zero">
        <xsl:with-param name="count" select="$count - 1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
