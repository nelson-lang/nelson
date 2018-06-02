//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "QtHighlighter.h"
#include "Keywords.hpp"
#include "Operators.hpp"
#include "QStringConverter.hpp"
#include "What.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
Highlighter::Highlighter(QTextDocument* parent) : QSyntaxHighlighter(parent)
{
    HighlightingRule rule;
    keywordFormat.setForeground(Qt::darkBlue);
    keywordFormat.setFontWeight(QFont::Bold);
    QStringList keywordPatterns;
    wstringVector keywordsVector = GetKeywords();
    for (size_t k = 0; k < keywordsVector.size(); k++) {
        keywordPatterns << wstringToQString(L"\\b" + keywordsVector[k] + L"\\b");
    }
    foreach (const QString& pattern, keywordPatterns) {
        rule.pattern = QRegularExpression(pattern);
        rule.format = keywordFormat;
        highlightingRules.append(rule);
    }
    builtinFormat.setForeground(Qt::darkCyan);
    QStringList builtinPatterns;
    wstringVector builtinVector = WhatListOfBuiltin();
    for (size_t k = 0; k < builtinVector.size(); k++) {
        builtinPatterns << wstringToQString(L"\\b" + builtinVector[k] + L"\\b");
    }
    foreach (const QString& pattern, builtinPatterns) {
        rule.pattern = QRegularExpression(pattern);
        rule.format = builtinFormat;
        highlightingRules.append(rule);
    }
    macroFormat.setForeground(Qt::darkCyan);
    QStringList macroPatterns;
    wstringVector macroVector = WhatListOfMacro();
    for (size_t k = 0; k < macroVector.size(); k++) {
        macroPatterns << wstringToQString(L"\\b" + macroVector[k] + L"\\b");
    }
    foreach (const QString& pattern, macroPatterns) {
        rule.pattern = QRegularExpression(pattern);
        rule.format = macroFormat;
        highlightingRules.append(rule);
    }
    singleLineCommentFormat.setForeground(Qt::darkGreen);
    rule.pattern = QRegularExpression("//[^\n]*");
    rule.format = singleLineCommentFormat;
    highlightingRules.append(rule);
    singleLineCommentFormat.setForeground(Qt::darkGreen);
    rule.pattern = QRegularExpression("%[^\n]*");
    rule.format = singleLineCommentFormat;
    highlightingRules.append(rule);
    quotationFormat.setForeground(Qt::darkMagenta);
    rule.pattern = QRegularExpression("'.*'");
    rule.format = quotationFormat;
    highlightingRules.append(rule);
    isEnabled = false;
}
//=============================================================================
void
Highlighter::highlightBlock(const QString& text)
{
    if (text.isEmpty() || (!isEnabled)) {
        return;
    }
    foreach (const HighlightingRule& rule, highlightingRules) {
        QRegularExpressionMatchIterator matchIterator = rule.pattern.globalMatch(text);
        while (matchIterator.hasNext()) {
            QRegularExpressionMatch match = matchIterator.next();
            setFormat(match.capturedStart(), match.capturedLength(), rule.format);
        }
    }
}
//=============================================================================
void
Highlighter::setEnable(bool _enable)
{
    isEnabled = _enable;
}
//=============================================================================
bool
Highlighter::getEnable()
{
    return isEnabled;
}
//=============================================================================
