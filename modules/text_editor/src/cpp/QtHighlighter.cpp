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
#include "NelsonPalette.hpp"
#include "Operators.hpp"
#include "QStringConverter.hpp"
#include "What.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
Highlighter::Highlighter(QTextDocument* parent) : QSyntaxHighlighter(parent)
{
    const bool darkMode = Nelson::isDarkPalette();

    keywordColor = darkMode ? QColor(QStringLiteral("#569CD6")) : QColor(Qt::darkBlue);
    builtinColor = darkMode ? QColor(QStringLiteral("#4EC9B0")) : QColor(Qt::darkCyan);
    macroColor = builtinColor;
    singleLineCommentColor = darkMode ? QColor(QStringLiteral("#6A9955")) : QColor(Qt::darkGreen);
    multilineCommentColor = singleLineCommentColor;
    quotationColor = darkMode ? QColor(QStringLiteral("#CE9178")) : QColor(Qt::darkMagenta);
    operatorColor = darkMode ? QColor(QStringLiteral("#D4D4D4")) : QColor(Qt::black);

    keywordFormat.setForeground(keywordColor);
    keywordFormat.setFontWeight(QFont::Bold);
    builtinFormat.setForeground(builtinColor);
    macroFormat.setForeground(macroColor);
    singleLineCommentFormat.setForeground(singleLineCommentColor);
    multilineCommentFormat.setForeground(multilineCommentColor);
    quotationFormat.setForeground(quotationColor);

    // one single regular expression tokenizes the identifiers of a line,
    // the identifier kind is then resolved with a hash lookup.
    identifierExpression = QRegularExpression(QStringLiteral("\\b[A-Za-z_]\\w*\\b"));
    identifierExpression.optimize();

    wstringVector keywordsVector = GetKeywords();
    for (auto& k : keywordsVector) {
        keywords.insert(wstringToQString(k));
    }
    // "arguments" is a contextual keyword not in the parser keyword table
    keywords.insert(QStringLiteral("arguments"));

    wstringVector builtinVector = WhatListOfBuiltin();
    for (auto& k : builtinVector) {
        builtins.insert(wstringToQString(k));
    }

    wstringVector macroVector = WhatListOfMacro();
    for (auto& k : macroVector) {
        macros.insert(wstringToQString(k));
    }

    HighlightingRule rule;
    rule.pattern = QRegularExpression(QStringLiteral("%[^\n]*"));
    rule.pattern.optimize();
    rule.format = singleLineCommentFormat;
    highlightingRules.append(rule);

    rule.pattern = QRegularExpression(QStringLiteral("%\\{[\\s\\S]*?%\\}"));
    rule.pattern.optimize();
    rule.format = multilineCommentFormat;
    highlightingRules.append(rule);

    rule.pattern = QRegularExpression(QStringLiteral("'.*'"));
    rule.pattern.optimize();
    rule.format = quotationFormat;
    highlightingRules.append(rule);
    isEnabled = false;
}
//=============================================================================
void
Highlighter::applyIdentifierRules(const QString& segment, int offset)
{
    QRegularExpressionMatchIterator matchIterator = identifierExpression.globalMatch(segment);
    while (matchIterator.hasNext()) {
        QRegularExpressionMatch match = matchIterator.next();
        const QString identifier = match.captured();
        if (keywords.contains(identifier)) {
            setFormat(offset + match.capturedStart(), match.capturedLength(), keywordFormat);
        } else if (builtins.contains(identifier)) {
            setFormat(offset + match.capturedStart(), match.capturedLength(), builtinFormat);
        } else if (macros.contains(identifier)) {
            setFormat(offset + match.capturedStart(), match.capturedLength(), macroFormat);
        }
    }
}
//=============================================================================
void
Highlighter::applyRulesOnRange(const QString& segment, int offset)
{
    applyIdentifierRules(segment, offset);
    foreach (const HighlightingRule& rule, highlightingRules) {
        QRegularExpressionMatchIterator matchIterator = rule.pattern.globalMatch(segment);
        while (matchIterator.hasNext()) {
            QRegularExpressionMatch match = matchIterator.next();
            setFormat(offset + match.capturedStart(), match.capturedLength(), rule.format);
        }
    }
}
//=============================================================================
void
Highlighter::highlightBlock(const QString& text)
{
    if (text.isEmpty() || (!isEnabled)) {
        return;
    }

    static const QRegularExpression startExpr(QStringLiteral("%\\{"));
    static const QRegularExpression endExpr(QStringLiteral("%\\}"));

    int startIndex = -1;
    if (previousBlockState() != 1) {
        QRegularExpressionMatch startMatch = startExpr.match(text);
        startIndex = startMatch.hasMatch() ? startMatch.capturedStart() : -1;
    } else {
        startIndex = 0;
    }

    int endIndex = -1;
    if (startIndex >= 0) {
        QRegularExpressionMatch endMatch
            = endExpr.match(text, startIndex + (startIndex == 0 ? 0 : 2));
        endIndex = endMatch.hasMatch() ? endMatch.capturedEnd() : -1;
    } else {
        QRegularExpressionMatch endMatch = endExpr.match(text);
        endIndex = endMatch.hasMatch() ? endMatch.capturedEnd() : -1;
    }

    if (startIndex >= 0) {
        if (endIndex >= 0 && endIndex > startIndex) {
            setFormat(startIndex, endIndex - startIndex, multilineCommentFormat);
            setCurrentBlockState(0);
            if (endIndex < text.length()) {
                QString rest = text.mid(endIndex);
                applyRulesOnRange(rest, endIndex);
            }
        } else {
            setFormat(startIndex, text.length() - startIndex, multilineCommentFormat);
            setCurrentBlockState(1);
            if (startIndex > 0) {
                QString before = text.left(startIndex);
                applyRulesOnRange(before, 0);
            }
        }
    } else if (previousBlockState() == 1) {
        if (endIndex >= 0) {
            setFormat(0, endIndex, multilineCommentFormat);
            setCurrentBlockState(0);
            if (endIndex < text.length()) {
                QString rest = text.mid(endIndex);
                applyRulesOnRange(rest, endIndex);
            }
        } else {
            setFormat(0, text.length(), multilineCommentFormat);
            setCurrentBlockState(1);
        }
    } else {
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
