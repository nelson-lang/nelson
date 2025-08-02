//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    for (auto& k : keywordsVector) {
        keywordPatterns << wstringToQString(L"\\b" + k + L"\\b");
    }
    foreach (const QString& pattern, keywordPatterns) {
        rule.pattern = QRegularExpression(pattern);
        rule.format = keywordFormat;
        highlightingRules.append(rule);
    }
    builtinFormat.setForeground(Qt::darkCyan);
    QStringList builtinPatterns;
    wstringVector builtinVector = WhatListOfBuiltin();
    for (auto& k : builtinVector) {
        builtinPatterns << wstringToQString(L"\\b" + k + L"\\b");
    }
    foreach (const QString& pattern, builtinPatterns) {
        rule.pattern = QRegularExpression(pattern);
        rule.format = builtinFormat;
        highlightingRules.append(rule);
    }
    macroFormat.setForeground(Qt::darkCyan);
    QStringList macroPatterns;
    wstringVector macroVector = WhatListOfMacro();
    for (auto& k : macroVector) {
        macroPatterns << wstringToQString(L"\\b" + k + L"\\b");
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
