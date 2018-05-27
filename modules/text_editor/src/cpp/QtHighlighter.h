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
