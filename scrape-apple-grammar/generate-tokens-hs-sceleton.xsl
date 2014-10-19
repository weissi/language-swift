<?xml version="1.0"?>
<xsl:stylesheet version="1.1"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:str="http://exslt.org/strings">
  <xsl:output method="text"/>
  <xsl:strip-space elements="*"/>

  <xsl:variable name='text' select='"select-statment"' />
  <xsl:variable name='lowers' select='"abcdefghijklmnopqrstuvwxyz"' />
  <xsl:variable name='uppers' select='"ABCDEFGHIJKLMNOPQRSTUVWXYZ"' />

  <xsl:template match="/grammar">

    <xsl:call-template name="datatype"/>

    <xsl:text>&#xa;</xsl:text>

    <xsl:call-template name="span-of-tok"/>

    <xsl:text>&#xa;</xsl:text>

    <xsl:call-template name="show-instance"/>

  </xsl:template>

  <xsl:template name="datatype">
    <xsl:text>&#xa;data Token&#xa;</xsl:text>
    <xsl:apply-templates mode="define-hs-token-data-type"/>
  </xsl:template>

  <xsl:template name="span-of-tok">
    <xsl:text>spanOfToken :: Token -> TokenSpan&#xa;</xsl:text>
    <xsl:apply-templates mode="define-span-of-tok"/>
  </xsl:template>

  <xsl:template name="show-instance">
    <xsl:text>instance Show Token where&#xa;</xsl:text>
    <xsl:apply-templates mode="define-show-instance"/>
  </xsl:template>

  <!-- HASKELL TOKEN DATATYPE -->
  <xsl:template match="production" mode="define-hs-token-data-type">
    <xsl:apply-templates select="alternative/literal"
                         mode="define-hs-token-data-type"/>
  </xsl:template>

  <xsl:template match="literal" mode="define-hs-token-data-type">
    <xsl:variable name="text" select="text()"/>
    <xsl:if test="not(preceding::literal[text()=$text])">
      <xsl:text>    </xsl:text>
      <xsl:choose>
        <xsl:when test="(preceding::literal)">
          <xsl:text>| </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>= </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="replace-specials-and-camel-case">
        <xsl:with-param name="prefix" select="'Token'"/>
        <xsl:with-param name="text" select="$text"/>
      </xsl:call-template>
      <xsl:text> { span :: !TokenSpan }    -- </xsl:text>
      <xsl:value-of select="text()" />
      <xsl:text>&#xa;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- SPAN-OF-TOK FUNCTION -->
  <xsl:template match="production" mode="define-span-of-tok">
    <xsl:apply-templates select="alternative/literal"
                         mode="define-span-of-tok"/>
  </xsl:template>

  <xsl:template match="literal" mode="define-span-of-tok">
    <xsl:variable name="text" select="text()"/>
    <xsl:if test="not(preceding::literal[text()=$text])">
      <xsl:text>spanOfToken (</xsl:text>
      <xsl:call-template name="replace-specials-and-camel-case">
        <xsl:with-param name="prefix" select="'Token'"/>
        <xsl:with-param name="text" select="$text"/>
      </xsl:call-template>
      <xsl:text> span) = span&#xa;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- SHOW INSTANCE -->
  <xsl:template match="production" mode="define-show-instance">
    <xsl:apply-templates select="alternative/literal"
                         mode="define-show-instance"/>
  </xsl:template>

  <xsl:template match="literal" mode="define-show-instance">
    <xsl:variable name="text" select="text()"/>
    <xsl:if test="not(preceding::literal[text()=$text])">
      <xsl:text>  showsPrec _ (</xsl:text>
      <xsl:call-template name="replace-specials-and-camel-case">
        <xsl:with-param name="prefix" select="'Token'"/>
        <xsl:with-param name="text" select="$text"/>
      </xsl:call-template>
      <xsl:text> _) = showString "</xsl:text>
      <xsl:value-of select="text()"/>
      <xsl:text>"&#xa;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- TOKEN LIST -->
  <xsl:template match="production" mode="list-tokens">
    <xsl:apply-templates select="alternative/literal" mode="list-tokens"/>
  </xsl:template>

  <xsl:template match="literal" mode="list-tokens">
    <xsl:variable name="text" select="text()"/>
    <xsl:if test="not(preceding::literal[text()=$text])">
      <xsl:text>    '</xsl:text>
      <xsl:value-of select="$text"/>
      <xsl:text>'    { Loc $$ </xsl:text>
      <xsl:call-template name="replace-specials-and-camel-case">
        <xsl:with-param name="prefix" select="'Token'"/>
        <xsl:with-param name="text" select="$text"/>
      </xsl:call-template>
      <xsl:text> }&#xa;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- HELPERS -->
  <xsl:template name="replace-specials">
    <xsl:param name="text"/>

    <xsl:value-of select="
                  str:replace( str:replace( str:replace( str:replace( str:replace(
                  str:replace( str:replace( str:replace( str:replace( str:replace(
                  str:replace( str:replace( str:replace( str:replace( str:replace(
                  str:replace( str:replace( str:replace( str:replace( str:replace(
                  str:replace( str:replace( str:replace( str:replace( str:replace(
                  str:replace( str:replace( str:replace( str:replace( str:replace(
                  str:replace( str:replace( str:replace( str:replace( str:replace(
                  str:replace( str:replace(
                    $text,
                    '__', ''),
                    '%', 'Percent'),
                    '\', 'BS'),
                    '`', 'Backtick'),
                    ';', 'Semicolon'),
                    '(', 'LParen'),
                    ')', 'RParen'),
                    '{', 'LBrace'),
                    '}', 'RBrace'),
                    ':', 'Colon'),
                    ',', 'Comma'),
                    '&lt;', 'LT'),
                    '>', 'GT'),
                    '==', 'DoubleEq'),
                    '.', 'Dot'),
                    '=', 'Eq'),
                    '->', 'Arrow'),
                    '...', 'TripleDot'),
                    '#', 'Hash'),
                    '_', 'Underscore'),
                    '@', 'At'),
                    '[', 'LBracket'),
                    ']', 'RBracket'),
                    '&amp;', 'Amp'),
                    '?', 'Questionmark'),
                    '!', 'Bang'),
                    '$', 'Dollar'),
                    '+', 'Plus'),
                    '-', 'Minus'),
                    '&quot;', 'DQuotes'),
                    'apos', 'Apos'),
                    '/', 'Div'),
                    '*', 'Times'),
                    '|', 'Pipe'),
                    '^', 'Caret'),
                    '~', 'Tilde'),
                    '..', 'DoubleDot')
                    " />
  </xsl:template>

  <xsl:template name="camel-case">
    <xsl:param name="text"/>
    <xsl:param name="prefix"/>

    <xsl:for-each select="str:split($text, '-')">
      <xsl:value-of select="
                    concat(
                    translate(substring(., 1, 1), $lowers, $uppers),
                    translate(substring(., 2),    $uppers, $lowers),
                    ''
                    )"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="replace-specials-and-camel-case">
    <xsl:param name="text"/>
    <xsl:param name="prefix"/>

    <xsl:value-of select="$prefix"/>
    <xsl:variable name="text-no-specials">
      <xsl:call-template name="replace-specials">
        <xsl:with-param name="text" select="$text"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="text-fixup">
      <xsl:choose>
        <xsl:when test="$text = 'e'">
          <xsl:text>TokenELowercase</xsl:text>
        </xsl:when>
        <xsl:when test="$text = 'p'">
          <xsl:text>TokenPLowercase</xsl:text>
        </xsl:when>
        <xsl:when test="$text = 'protocol'">
          <xsl:text>TokenProtocolLowercase</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$text-no-specials"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:for-each select="str:split($text-fixup, '-')">
      <xsl:value-of select="
                    concat(
                    translate(substring(., 1, 1), $lowers, $uppers),
                    substring(., 2),
                    ''
                    )"/>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
