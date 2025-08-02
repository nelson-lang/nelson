//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtCore/QRegularExpression>
#include <QtGui/QSyntaxHighlighter>
#include <QtGui/QTextCharFormat>
//=============================================================================
class QTextDocument;
//=============================================================================
#define DEFAULT_QT_KEYWORD_COLOR Qt::darkBlue
#define DEFAULT_QT_BUILTIN_COLOR Qt::darkBlue
#define DEFAULT_QT_MACRO_COLOR Qt::darkBlue
#define DEFAULT_QT_SINGLE_LINE_COMMENT_COLOR Qt::darkBlue
#define DEFAULT_QT_OPERATOR_COLOR Qt::darkBlue
#define DEFAULT_QT_UNFINISHED_STRING_COLOR Qt::darkBlue
#define DEFAULT_QT_QUOTATION_COLOR Qt::darkBlue
//=============================================================================
class Highlighter : public QSyntaxHighlighter
{
    Q_OBJECT

public:
    Highlighter(QTextDocument* parent = 0);
    void
    setEnable(bool _enable);
    bool
    getEnable();

protected:
    void
    highlightBlock(const QString& text) override;

private:
    struct HighlightingRule
    {
        QRegularExpression pattern;
        QTextCharFormat format;
    };
    QVector<HighlightingRule> highlightingRules;

    QTextCharFormat keywordFormat;
    QTextCharFormat builtinFormat;
    QTextCharFormat macroFormat;
    QTextCharFormat singleLineCommentFormat;
    QTextCharFormat quotationFormat;

    bool isEnabled;
    QColor keywordColor;
    QColor builtinColor;
    QColor macroColor;
    QColor operatorColor;
    QColor singleLineCommentColor;
    QColor quotationColor;
};
//=============================================================================
