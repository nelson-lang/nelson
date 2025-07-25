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
} // namespace Nelson
//=============================================================================
