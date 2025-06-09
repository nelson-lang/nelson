//=============================================================================
#include "VariableDefaultModel.h"
#include "Exception.hpp"
#include "ClassName.hpp"
#include <QtWidgets/QTextEdit>
#include <QtCore/QTextStream>
#include <QtCore/QRegularExpression>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocument>
#include <QtWidgets/QScrollBar>
#include <sstream>
#include <iomanip>
#include "EvaluateCommand.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
VariableDefaultModel::VariableDefaultModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QTextEdit* textEdit, QObject* parent)
    : QObject(parent)
    , m_array(array)
    , m_variableName(name)
    , m_evaluator(evaluator)
    , m_textEdit(textEdit)
    , m_persistenceTimer(new QTimer(this))
{
    m_persistenceTimer->setSingleShot(true);
    m_persistenceTimer->setInterval(MIN_UPDATE_INTERVAL_MS);
    connect(m_persistenceTimer, &QTimer::timeout, this,
        &VariableDefaultModel::performBatchedPersistence);

    initializeContent();
    connectTextEditSignals();

    // Determine if the variable should be read-only
    m_isReadOnly = !canEdit();
    if (m_textEdit) {
        m_textEdit->setReadOnly(m_isReadOnly);
    }
}

//=============================================================================
void
VariableDefaultModel::initializeContent()
{
    m_ContentAsString = formatArrayContent(m_array);
    m_hasValidContent = true;

    if (m_textEdit) {
        m_suppressSignals = true;
        m_textEdit->setPlainText(m_ContentAsString);
        m_suppressSignals = false;
    }
}

//=============================================================================
void
VariableDefaultModel::connectTextEditSignals()
{
    if (m_textEdit) {
        connect(m_textEdit, &QTextEdit::textChanged, this, &VariableDefaultModel::onTextChanged);
    }
}

//=============================================================================
void
VariableDefaultModel::disconnectTextEditSignals()
{
    if (m_textEdit) {
        disconnect(m_textEdit, &QTextEdit::textChanged, this, &VariableDefaultModel::onTextChanged);
    }
}

//=============================================================================
void
VariableDefaultModel::refreshFromArray(const ArrayOf& value)
{
    m_array = value;
    initializeContent();
    emit contentChanged();
}

//=============================================================================
bool
VariableDefaultModel::isStructureCompatible(const ArrayOf& value) const
{
    return true;
}
//=============================================================================
void
VariableDefaultModel::forceViewUpdate()
{
    if (m_textEdit) {
        m_suppressSignals = true;
        QString currentText = m_textEdit->toPlainText();
        QTextCursor cursor = m_textEdit->textCursor();
        int scrollValue
            = m_textEdit->verticalScrollBar() ? m_textEdit->verticalScrollBar()->value() : 0;

        m_textEdit->setPlainText(m_ContentAsString);
        m_textEdit->setTextCursor(cursor);

        if (m_textEdit->verticalScrollBar()) {
            m_textEdit->verticalScrollBar()->setValue(scrollValue);
        }

        m_suppressSignals = false;
    }
}

//=============================================================================
QString
VariableDefaultModel::formatArrayContent(const ArrayOf& array) const
{
    std::wstring command = L"display(" + array.wname() + L", 'val');";
    std::wstring result;
    if (m_evaluator) {
        EvaluateConsoleCommandToString(m_evaluator, command, result);
        return wstringToQString(result);
    }
    return QString();
}
//=============================================================================
void
VariableDefaultModel::onTextChanged()
{
    if (m_suppressSignals || m_isReadOnly) {
        return;
    }
}

//=============================================================================
void
VariableDefaultModel::schedulePeristenceUpdate()
{
    m_pendingChanges = true;
    m_persistenceTimer->start();
}

//=============================================================================
void
VariableDefaultModel::performBatchedPersistence()
{
    if (!m_pendingChanges || m_isReadOnly) {
        return;
    }

    // if (setContentFromText(m_ContentAsString)) {
    //     persistChangesToContext();
    //     m_pendingChanges = false;
    //     emit variableModified(m_variableName);
    // } else {
    //     emit errorOccurred(tr("Failed to parse text content for variable:
    //     %1").arg(m_variableName));
    // }
}

//=============================================================================
bool
VariableDefaultModel::setContentFromText(const QString& text)
{
    return false;
}
//=============================================================================
bool
VariableDefaultModel::canEdit() const
{
    return false;
}
//=============================================================================
bool
VariableDefaultModel::isReadOnly() const
{
    return m_isReadOnly;
}
//=============================================================================
QString
VariableDefaultModel::getVariableClassAsString() const
{
    std::wstring classname;
    ClassName(m_array, classname);
    return wstringToQString(classname);
}
//=============================================================================
QString
VariableDefaultModel::getVariableDimensionsAsString() const
{
    Dimensions dims = m_array.getDimensions();
    return wstringToQString(dims.toWideString());
}
//=============================================================================
QString
VariableDefaultModel::getFormattedContent() const
{
    return m_ContentAsString;
}

//=============================================================================
QString
VariableDefaultModel::getPlainContent() const
{
    return m_textEdit ? m_textEdit->toPlainText() : m_ContentAsString;
}

//=============================================================================
QString
VariableDefaultModel::getSelectedText() const
{
    if (m_textEdit && m_textEdit->textCursor().hasSelection()) {
        return m_textEdit->textCursor().selectedText();
    }
    return QString();
}

//=============================================================================
QString
VariableDefaultModel::getAllText() const
{
    return getPlainContent();
}

//=============================================================================
bool
VariableDefaultModel::pasteText(const QString& text, int position)
{
    if (!m_textEdit || m_isReadOnly) {
        return false;
    }

    QTextCursor cursor = m_textEdit->textCursor();
    if (position >= 0) {
        cursor.setPosition(position);
    }

    cursor.insertText(text);
    return true;
}

//=============================================================================
bool
VariableDefaultModel::insertText(const QString& text, int position)
{
    return pasteText(text, position);
}

//=============================================================================
bool
VariableDefaultModel::replaceText(const QString& text, int start, int length)
{
    if (!m_textEdit || m_isReadOnly) {
        return false;
    }

    QTextCursor cursor = m_textEdit->textCursor();
    cursor.setPosition(start);

    if (length > 0) {
        cursor.setPosition(start + length, QTextCursor::KeepAnchor);
    } else if (length < 0) {
        cursor.select(QTextCursor::Document);
    }

    cursor.insertText(text);
    return true;
}

//=============================================================================
bool
VariableDefaultModel::isValidTextContent(const QString& text) const
{
    return text.length() <= MAX_CONTENT_SIZE;
}

//=============================================================================
bool
VariableDefaultModel::canParseAsNumeric(const QString& text) const
{
    // Simple check - could be enhanced
    return text.contains(QRegularExpression("[0-9]"));
}

//=============================================================================
bool
VariableDefaultModel::canParseAsString(const QString& text) const
{
    return true; // Most text can be treated as string
}
//=============================================================================
} // namespace Nelson
//=============================================================================
