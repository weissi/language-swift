<?xml version="1.0"?>
<xsl:stylesheet version="1.1"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:math="http://exslt.org/math">
  <xsl:output method="xml" indent="yes" />

  <xsl:template match="/grammar">
    <grammar>
      <xsl:apply-templates/>
    </grammar>
  </xsl:template>

  <xsl:template match="production">
    <production name="{@name}">
      <xsl:for-each select="alternative">
        <xsl:call-template name="de-optionalise-alternative">
          <xsl:with-param name="node" select="."/>
        </xsl:call-template>
      </xsl:for-each>
    </production>
  </xsl:template>

  <xsl:template name="de-optionalise-alternative">
    <xsl:param name="node"/>
    <xsl:param name="enabled-opts" select="0"/>

    <xsl:variable name="num-opts" select="count($node/optional)"/>

    <alternative enabled-optional-bits="{$enabled-opts}">
      <xsl:for-each select="*">
        <xsl:choose>
          <xsl:when test="name() = 'optional'">
            <xsl:choose>
              <xsl:when test="($enabled-opts mod ( math:power(2, @index) * 2) ) -
                        ($enabled-opts mod ( math:power(2, @index)    ) )">
                <xsl:copy-of select="./*"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:comment>disabled optional</xsl:comment>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </alternative>

    <xsl:if test="math:power(2, $num-opts) - 1 > $enabled-opts">
      <xsl:call-template name="de-optionalise-alternative">
        <xsl:with-param name="node" select="$node"/>
        <xsl:with-param name="enabled-opts" select="$enabled-opts+1"/>
      </xsl:call-template>
    </xsl:if>

  </xsl:template>

</xsl:stylesheet>
