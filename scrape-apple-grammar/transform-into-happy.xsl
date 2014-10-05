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
    <xsl:call-template name="start-hs-preamble"/>

    <xsl:call-template name="end-hs-preamble-and-start-data-section"/>

    <xsl:text>%token&#xa;</xsl:text>
    <xsl:apply-templates mode="list-tokens"/>

    <xsl:text>&#xa;%%&#xa;&#xa;</xsl:text>

    <xsl:apply-templates mode="output-grammar"/>

    <xsl:call-template name="postamble"/>


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
      <xsl:text>&#xa;</xsl:text>
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

<!-- THE BNF-LIKE GRAMMAR -->
  <xsl:template match="production" mode="output-grammar">
    <xsl:call-template name="camel-case">
      <xsl:with-param name="text" select="@name"/>
    </xsl:call-template>
    <xsl:text>&#xa;    :</xsl:text>
    <xsl:apply-templates mode="output-grammar"/>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="alternative" mode="output-grammar">
    <xsl:if test="preceding-sibling::alternative">
      <xsl:text>&#xa;    |</xsl:text>
    </xsl:if>
    <xsl:apply-templates mode="output-grammar"/>
    <xsl:text>    { undefined :: () }</xsl:text>
  </xsl:template>

  <xsl:template match="non-terminal" mode="output-grammar">
    <xsl:text> </xsl:text>
    <xsl:call-template name="camel-case">
      <xsl:with-param name="text" select="text()"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="literal" mode="output-grammar">
    <xsl:text> '</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>'</xsl:text>
  </xsl:template>

  <xsl:template match="text-description" mode="output-grammar">
    <xsl:text> {- TEXT-DESCRIPTION (</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>) -}</xsl:text>
  </xsl:template>

  <!-- OTHER STUFF -->
  <xsl:template name="start-hs-preamble">
    <xsl:text>
{

module Language.Swift.HappyParser where

import Data.Char

    </xsl:text>
  </xsl:template>


  <xsl:template name="end-hs-preamble-and-start-data-section">
    <xsl:text>

}

%name calc
%tokentype { Token }
%error { parseError }
-- %monad { P }
-- %lexer { lexer } { Loc _ EOF }
    </xsl:text>
  </xsl:template>

  <xsl:template name="postamble">
    <xsl:text>
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

    </xsl:text>

    <xsl:text>&#xa;data Token&#xa;</xsl:text>
    <xsl:apply-templates mode="define-hs-token-data-type"/>

    <xsl:text>&#xa;</xsl:text>
    <xsl:text>
data Loc a = Loc
    { loc :: SrcSpan
    , unLoc :: a
    }
    deriving (Eq,Ord,Show)

data SrcSpan = SrcSpan
    { srcSpanFilename    :: String
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
  deriving (Show, Eq, Ord)
    </xsl:text>

    <xsl:text>&#xa;}&#xa;</xsl:text>
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
