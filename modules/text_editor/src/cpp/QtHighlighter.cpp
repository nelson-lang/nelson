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
    rule.pattern = QRegularExpression("%[^\n]*");
    rule.format = singleLineCommentFormat;
    highlightingRules.append(rule);

    // Multiline comment %{ ... %}
    multilineCommentFormat.setForeground(Qt::darkGreen);
    multilineCommentColor = Qt::darkGreen;
    rule.pattern = QRegularExpression("%\\{[\\s\\S]*?%\\}");
    rule.format = multilineCommentFormat;
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

    QRegularExpression startExpr("%\\{");
    QRegularExpression endExpr("%\\}");

    auto applyRulesOnRange = [&](const QString& segment, int offset) {
        foreach (const HighlightingRule& rule, highlightingRules) {
            QRegularExpressionMatchIterator matchIterator = rule.pattern.globalMatch(segment);
            while (matchIterator.hasNext()) {
                QRegularExpressionMatch match = matchIterator.next();
                setFormat(offset + match.capturedStart(), match.capturedLength(), rule.format);
            }
        }
    };

    // If we are not already inside a multiline comment, look for an opening marker
    int startIndex = -1;
    if (previousBlockState() != 1) {
        QRegularExpressionMatch startMatch = startExpr.match(text);
        startIndex = startMatch.hasMatch() ? startMatch.capturedStart() : -1;
    } else {
        startIndex = 0; // continuation of a previous block
    }

    // Find a closing marker after startIndex when possible
    int endIndex = -1;
    if (startIndex >= 0) {
        QRegularExpressionMatch endMatch
            = endExpr.match(text, startIndex + (startIndex == 0 ? 0 : 2));
        endIndex = endMatch.hasMatch() ? endMatch.capturedEnd() : -1;
    } else {
        // Not starting a new block; look for any closing marker (for continued block)
        QRegularExpressionMatch endMatch = endExpr.match(text);
        endIndex = endMatch.hasMatch() ? endMatch.capturedEnd() : -1;
    }

    if (startIndex >= 0) {
        // We have an opening on this line (or continuation)
        if (endIndex >= 0 && endIndex > startIndex) {
            // Comment opens and closes on this line
            setFormat(startIndex, endIndex - startIndex, multilineCommentFormat);
            setCurrentBlockState(0);
            // Highlight the rest of the line after the comment
            if (endIndex < text.length()) {
                QString rest = text.mid(endIndex);
                applyRulesOnRange(rest, endIndex);
            }
        } else {
            // Comment opens but does not close on this line
            setFormat(startIndex, text.length() - startIndex, multilineCommentFormat);
            setCurrentBlockState(1);
            // Highlight the portion before the comment
            if (startIndex > 0) {
                QString before = text.left(startIndex);
                applyRulesOnRange(before, 0);
            }
        }
    } else if (previousBlockState() == 1) {
        // This block is a continuation from previous line
        if (endIndex >= 0) {
            // Comment closes on this line
            setFormat(0, endIndex, multilineCommentFormat);
            setCurrentBlockState(0);
            // Highlight the remainder after the closing marker
            if (endIndex < text.length()) {
                QString rest = text.mid(endIndex);
                applyRulesOnRange(rest, endIndex);
            }
        } else {
            // Still inside multiline comment
            setFormat(0, text.length(), multilineCommentFormat);
            setCurrentBlockState(1);
        }
    } else {
        // No multiline comment involvement on this line
        applyRulesOnRange(text, 0);
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
