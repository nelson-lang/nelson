//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtTextIndent.h"
#include "TextEditorIndentPreferences.h"
#include "NelsonFormattingTokens.hpp"
#include <QtCore/QString>
#include <QtGui/QTextBlock>
#include <QtGui/QTextDocument>
#include <QtGui/QTextDocumentFragment>
#include <algorithm>
//=============================================================================
// Indentation while typing.
//
// The full formatter (Nelson::smartIndentCode) cannot be used here: it parses
// and compiles the whole buffer and throws as soon as a block is not closed,
// which is the normal situation while the user is typing. The block level is
// therefore computed with the very same classification rules as
// NelsonCodeIndent.cpp (opener / middle / case / closer, switch aware) but
// without any parsing step, so an incomplete buffer is accepted.
//=============================================================================
namespace {
//=============================================================================
enum class ScanState
{
    Normal,
    SingleQuotedText,
    DoubleQuotedText,
    BlockComment
};
//=============================================================================
enum class LineKind
{
    Other,
    Opener,
    Middle,
    CaseOrOtherwise,
    Closer
};
//=============================================================================
static bool
isIdentifierCharacter(QChar character)
{
    return character.isLetterOrNumber() || (character == QLatin1Char('_'));
}
//=============================================================================
static bool
isSingleQuotedTextStart(const QString& text, int quotePosition)
{
    int previous = quotePosition;
    while ((previous > 0)
        && ((text.at(previous - 1) == QLatin1Char(' '))
            || (text.at(previous - 1) == QLatin1Char('\t')))) {
        --previous;
    }
    if (previous == 0) {
        return true;
    }
    const QChar character = text.at(previous - 1);
    if (isIdentifierCharacter(character) || (character == QLatin1Char(')'))
        || (character == QLatin1Char(']')) || (character == QLatin1Char('}'))) {
        return false;
    }
    return character != QLatin1Char('.');
}
//=============================================================================
static QString
stripCodeLine(const QString& line, ScanState& state)
{
    QString result;
    result.reserve(line.size());
    int k = 0;
    while (k < line.size()) {
        const QChar character = line.at(k);
        if (state == ScanState::BlockComment) {
            if ((character == QLatin1Char('%')) && (k + 1 < line.size())
                && (line.at(k + 1) == QLatin1Char('}'))) {
                state = ScanState::Normal;
                k += 2;
            } else {
                ++k;
            }
            continue;
        }
        if (state == ScanState::SingleQuotedText) {
            if ((character == QLatin1Char('\'')) && (k + 1 < line.size())
                && (line.at(k + 1) == QLatin1Char('\''))) {
                k += 2;
            } else {
                if (character == QLatin1Char('\'')) {
                    state = ScanState::Normal;
                }
                ++k;
            }
            continue;
        }
        if (state == ScanState::DoubleQuotedText) {
            if ((character == QLatin1Char('"')) && (k + 1 < line.size())
                && (line.at(k + 1) == QLatin1Char('"'))) {
                k += 2;
            } else {
                if (character == QLatin1Char('"')) {
                    state = ScanState::Normal;
                }
                ++k;
            }
            continue;
        }
        if ((character == QLatin1Char('%')) && (k + 1 < line.size())
            && (line.at(k + 1) == QLatin1Char('{'))) {
            state = ScanState::BlockComment;
            k += 2;
            continue;
        }
        if ((character == QLatin1Char('%'))
            || ((character == QLatin1Char('/')) && (k + 1 < line.size())
                && (line.at(k + 1) == QLatin1Char('/')))) {
            break;
        }
        if ((character == QLatin1Char('\'')) && isSingleQuotedTextStart(line, k)) {
            state = ScanState::SingleQuotedText;
            ++k;
            continue;
        }
        if (character == QLatin1Char('"')) {
            state = ScanState::DoubleQuotedText;
            ++k;
            continue;
        }
        result += character;
        ++k;
    }
    return result;
}
//=============================================================================
static QString
firstIdentifier(const QString& code)
{
    const QString trimmed = code.trimmed();
    int k = 0;
    while ((k < trimmed.size()) && isIdentifierCharacter(trimmed.at(k))) {
        ++k;
    }
    return trimmed.left(k);
}
//=============================================================================
static bool
isContextualBlockOpener(const QString& code, const QString& identifier)
{
    bool known = false;
    for (const char* keyword : Nelson::getFormattingContextualBlockKeywords()) {
        if (identifier == QLatin1String(keyword)) {
            known = true;
            break;
        }
    }
    if (!known) {
        return false;
    }
    // 'properties', 'methods', ... are contextual keywords: an assignment such
    // as 'properties = 3;' must not be seen as a block opener.
    const QString remainder = code.trimmed().mid(identifier.size());
    return !remainder.contains(QLatin1Char('='));
}
//=============================================================================
static LineKind
classifyLine(const QString& code)
{
    const QString identifier = firstIdentifier(code);
    if (identifier.isEmpty()) {
        return LineKind::Other;
    }
    if ((identifier == QLatin1String("else")) || (identifier == QLatin1String("elseif"))
        || (identifier == QLatin1String("catch"))) {
        return LineKind::Middle;
    }
    if ((identifier == QLatin1String("case")) || (identifier == QLatin1String("otherwise"))) {
        return LineKind::CaseOrOtherwise;
    }
    if (identifier == QLatin1String("end")) {
        return LineKind::Closer;
    }
    if (isContextualBlockOpener(code, identifier)) {
        return LineKind::Opener;
    }
    if ((identifier == QLatin1String("function")) || (identifier == QLatin1String("if"))
        || (identifier == QLatin1String("for")) || (identifier == QLatin1String("while"))
        || (identifier == QLatin1String("switch")) || (identifier == QLatin1String("try"))) {
        return LineKind::Opener;
    }
    return LineKind::Other;
}
//=============================================================================
static int
levelOfLine(LineKind kind, int level, const QList<int>& switchLevels)
{
    if (kind == LineKind::Closer) {
        const bool closesSwitch = !switchLevels.isEmpty() && (switchLevels.last() == level - 1);
        return closesSwitch ? std::max(switchLevels.last() - 1, 0) : std::max(level - 1, 0);
    }
    if (kind == LineKind::Middle) {
        return std::max(level - 1, 0);
    }
    if (kind == LineKind::CaseOrOtherwise) {
        return switchLevels.isEmpty() ? std::max(level - 1, 0) : switchLevels.last();
    }
    return level;
}
//=============================================================================
static void
advanceLevel(
    LineKind kind, const QString& code, int lineLevel, int& level, QList<int>& switchLevels)
{
    if (kind == LineKind::Closer) {
        const bool closesSwitch = !switchLevels.isEmpty() && (switchLevels.last() == level - 1);
        level = closesSwitch ? lineLevel : std::max(level - 1, 0);
        while (!switchLevels.isEmpty() && (switchLevels.last() > level)) {
            switchLevels.removeLast();
        }
    } else if (kind == LineKind::Middle) {
        level = std::max(level - 1, 0) + 1;
    } else if (kind == LineKind::CaseOrOtherwise) {
        level = lineLevel + 1;
    } else if (kind == LineKind::Opener) {
        if (firstIdentifier(code) == QLatin1String("switch")) {
            switchLevels.append(level + 1);
        }
        ++level;
    }
}
//=============================================================================
} // namespace
//=============================================================================
QtTextIndent::QtTextIndent(QObject* parent)
    : QObject(parent), m_te(nullptr), m_indentSize(TEXT_EDITOR_DEFAULT_INDENT_SIZE), m_enabled(true)
{
}
//=============================================================================
QtTextIndent::~QtTextIndent() = default;
//=============================================================================
void
QtTextIndent::setDocument(QtTextEdit* te)
{
    m_te = te;
}
//=============================================================================
QtTextEdit*
QtTextIndent::document() const
{
    return m_te;
}
//=============================================================================
void
QtTextIndent::setIndentSize(int indentSize)
{
    if ((indentSize > 0) && (indentSize <= TEXT_EDITOR_MAX_INDENT_SIZE)) {
        m_indentSize = indentSize;
    }
}
//=============================================================================
int
QtTextIndent::indentSize() const
{
    return m_indentSize;
}
//=============================================================================
void
QtTextIndent::setEnabled(bool enabled)
{
    m_enabled = enabled;
}
//=============================================================================
bool
QtTextIndent::isEnabled() const
{
    return m_enabled;
}
//=============================================================================
void
QtTextIndent::update()
{
    if ((m_te == nullptr) || !m_enabled) {
        return;
    }
    QTextDocument* doc = m_te->document();
    if (doc == nullptr) {
        return;
    }
    QTextCursor cursor(m_te->textCursor());
    QTextBlock block = cursor.block();
    if (!block.isValid()) {
        return;
    }
    const int blockNumber = block.blockNumber();

    ScanState state = ScanState::Normal;
    int level = 0;
    QList<int> switchLevels;
    QTextBlock current = doc->firstBlock();
    for (int k = 0; (k < blockNumber) && current.isValid(); ++k, current = current.next()) {
        const QString code = stripCodeLine(current.text(), state);
        const LineKind kind = classifyLine(code);
        const int lineLevel = levelOfLine(kind, level, switchLevels);
        advanceLevel(kind, code, lineLevel, level, switchLevels);
    }
    const QString lineText = block.text();
    const QString code = stripCodeLine(lineText, state);
    const int lineLevel = levelOfLine(classifyLine(code), level, switchLevels);

    const QString expectedIndent(lineLevel * m_indentSize, QLatin1Char(' '));
    int leading = 0;
    while ((leading < lineText.size())
        && ((lineText.at(leading) == QLatin1Char(' '))
            || (lineText.at(leading) == QLatin1Char('\t')))) {
        ++leading;
    }
    if (lineText.left(leading) == expectedIndent) {
        return;
    }
    const int blockPosition = block.position();
    const int positionInBlock = cursor.positionInBlock();
    QTextCursor editCursor(doc);
    editCursor.beginEditBlock();
    editCursor.setPosition(blockPosition);
    editCursor.setPosition(blockPosition + leading, QTextCursor::KeepAnchor);
    editCursor.insertText(expectedIndent);
    editCursor.endEditBlock();

    const int newPositionInBlock
        = std::max(positionInBlock - leading, 0) + static_cast<int>(expectedIndent.size());
    const int lastPosition = std::max(doc->characterCount() - 1, 0);
    QTextCursor finalCursor(doc);
    finalCursor.setPosition(
        std::min(std::max(blockPosition + newPositionInBlock, 0), lastPosition));
    m_te->setTextCursor(finalCursor);
}
//=============================================================================
