#!/bin/bash

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)
set -ue

trap 'echo UNEXPECTED ERROR; exit 1;' ERR

apple_grammar_html_url="https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html"
convert_xhtml_url='http://www.it.uc3m.es/jaf/cgi-bin/html2xhtml.cgi?linelength=120&input-charset=utf-8&output-charset=utf-8'

echo "--> Downloading Apple HTML"
tmp_html=$(mktemp /tmp/.tmp-html_XXXXXX)
curl "$apple_grammar_html_url" > "$tmp_html"

echo "--> Converting to XHTML"
tmp_xml=$(mktemp /tmp/.tmp-xml_XXXXXX)
curl --data-binary @"$tmp_html" \
    -H "Content-Type: text/html" \
    "$convert_xhtml_url" > "$tmp_xml"

rm "$tmp_html"
unset tmp_html

echo "--> Fixing XHTML"
tmp_xml_ok=$(mktemp /tmp/.tmp-xmlok_XXXXXX)
cat "$tmp_xml" | sed "s/&rsquo;/'/g" | sed 's/&shy;/ /g' | \
    sed 's/&hellip;/ /g' | sed 's# xmlns="http://www.w3.org/1999/xhtml"##g' \
    > "$tmp_xml_ok"
rm "$tmp_xml"
unset tmp_xml

echo "--> Validating XML"
xmllint "$tmp_xml_ok" > /dev/null

echo "--> Converting into XML grammar format"
tmp_xml_grammar=$(mktemp /tmp/.tmp-xmlgram_XXXXXX)
xsltproc --nonet "$HERE/convert-swift-grammar-xhtml.xsl" "$tmp_xml_ok" | \
    xsltproc "$HERE/factor-in-optional-symbols.xsl" - | \
    xmllint --format - \
    > "$HERE/gen/swift-grammar.xml"
rm "$tmp_xml_ok"
unset tmp_xml_ok

echo "--> Generating Happy Pre-code"
xsltproc "$HERE/transform-into-happy.xsl" "$HERE/gen/swift-grammar.xml" \
    > "$HERE/gen/swift-grammar.y"

echo "OK: Generated the following files"
ls -1 gen/*
