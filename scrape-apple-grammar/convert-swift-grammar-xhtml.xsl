<?xml version="1.0"?>
<xsl:stylesheet version="1.1"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                exclude-result-prefixes="xs">
  <xsl:output method="xml" indent="yes" />

  <xsl:template match="/">
    <grammar>
      <xsl:apply-templates select="/html/body/div[@class='content-wrapper']"/>
    </grammar>
  </xsl:template>

  <xsl:template match="/html/body/div[@class='content-wrapper']">
    <xsl:apply-templates select="div[@class='syntax-defs']"/>
  </xsl:template>

  <xsl:template match="div[@class='syntax-defs']">
    <xsl:apply-templates select="div[@class='syntax-defs-group']"
                         mode="syntax-defs-group"/>
  </xsl:template>

  <xsl:template match="*" mode="syntax-defs-group">
    <xsl:for-each select="p[@class='syntax-def']">
      <xsl:variable name="ctx" select="."/>
      <xsl:if test="not(preceding-sibling::p[@class='syntax-def' and
              normalize-space(span[@class='syntax-def-name']) =
              normalize-space($ctx/span[@class='syntax-def-name'])])">
        <xsl:apply-templates select="$ctx"
                             mode="syntax-def"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="*" mode="syntax-def">
    <production>
      <xsl:variable name="name">
        <xsl:value-of select="normalize-space(span[@class='syntax-def-name']/text())"/>
      </xsl:variable>
      <xsl:attribute name="name">
        <xsl:value-of select="$name"/>
      </xsl:attribute>
      <xsl:if test="count(span[@class='arrow']) != 1">
        <error type="more-than-one-arrow"/>
      </xsl:if>

      <!-- recurse down to the alternatives -->
      <xsl:if test="preceding-sibling::p[@class='syntax-def' and
              normalize-space(span[@class='syntax-def-name']) = $name]">
        <error type="syntax-def-called-more-than-just-for-first-production-per-name">
          <xsl:value-of select="$name"/>
        </error>
      </xsl:if>
      <xsl:call-template name="productions-with-alternatives"/>

      <!-- find all other productions with the same name -->
      <xsl:for-each select="following-sibling::p[@class='syntax-def' and
                    normalize-space(span[@class='syntax-def-name']) = $name]">
        <xsl:call-template name="productions-with-alternatives"/>
      </xsl:for-each>
    </production>
  </xsl:template>

  <xsl:template name="productions-with-alternatives">
    <xsl:variable name="directly-matching"
                  select="span[@class='arrow']/following-sibling::*[not(@class='alternative')]"/>
    <xsl:if test="$directly-matching">
      <xsl:call-template name="process-alternative">
        <xsl:with-param name="type" select="'direct'"/>
        <xsl:with-param name="nodes" select="$directly-matching"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:apply-templates select="span[@class='arrow']/following-sibling::*[@class='alternative']"
                         mode="in-production"/>
  </xsl:template>

  <xsl:template match="code[@class='literal']" mode="in-production">
    <literal><xsl:value-of select="normalize-space(text())"/></literal>
  </xsl:template>

  <xsl:template match="span[@class='syntactic-cat']" mode="in-production">
    <non-terminal>
      <xsl:value-of select="a/text()"/>
    </non-terminal>
  </xsl:template>

  <xsl:template match="span[@class='optional']" mode="in-production">
    <optional>
      <xsl:attribute name="index">
        <xsl:value-of select="count(preceding-sibling::span[@class='optional'])"/>
      </xsl:attribute>
      <xsl:apply-templates select="*" mode="in-production"/>
    </optional>
  </xsl:template>

  <xsl:template match="span[@class='alternative']" mode="in-production">
    <xsl:call-template name="process-alternative">
      <xsl:with-param name="type" select="'explicit-alternative'"/>
      <xsl:with-param name="nodes" select="*"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="process-alternative">
    <xsl:param name="type"/>
    <xsl:param name="nodes"/>

    <alternative type="{$type}">
      <xsl:apply-templates select="$nodes" mode="in-production"/>
    </alternative>
  </xsl:template>

  <xsl:template match="span[@class='text-description']" mode="in-production">
    <text-description>
      <xsl:value-of select="normalize-space(text())"/>
    </text-description>
  </xsl:template>

  <xsl:template match="sub[@class='subscript']" mode="in-production">
    <xsl:if test="normalize-space(text()) != 'opt'">
      <error type="unexpected-text-in-optional">
        <xsl:value-of select="text()"/>
      </error>
    </xsl:if>
  </xsl:template>

  <xsl:template match="*" mode="in-production">
    <error type="unrecognized-production-part">
      <xsl:copy-of select="."/>
    </error>
  </xsl:template>
</xsl:stylesheet>
