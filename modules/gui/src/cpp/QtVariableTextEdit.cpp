//=============================================================================
#include "QtVariableTextEdit.h"
#include <QtCore/QTimer>
#include <QtWidgets/QScrollBar>

//=============================================================================
namespace Nelson {
//=============================================================================
QtVariableTextEdit::QtVariableTextEdit(
    const QString& variableName, ArrayOf array, Evaluator* evaluator, QWidget* parent)
    : QTextEdit(parent)
{
    // Set up the text edit properties
    setReadOnly(true);
    setFont(QFont("Courier", 10)); // Monospace font for better formatting
    setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    // Create the model
    m_model = new VariableDefaultModel(variableName, array, evaluator, this, this);

    // Set up connections
    setupConnections();
}
//=============================================================================
void
QtVariableTextEdit::setupConnections()
{
    if (m_model) {
        connect(m_model, &VariableDefaultModel::contentChanged, this,
            &QtVariableTextEdit::onModelContentChanged);
        connect(
            m_model, &VariableDefaultModel::errorOccurred, this, &QtVariableTextEdit::onModelError);
        connect(m_model, &VariableDefaultModel::variableModified, this,
            &QtVariableTextEdit::onModelVariableModified);
    }
}
//=============================================================================
void
QtVariableTextEdit::refreshFromArray(const ArrayOf& value)
{
    if (m_model) {
        m_model->refreshFromArray(value);
    }
}
//=============================================================================
bool
QtVariableTextEdit::isStructureCompatible(const ArrayOf& value) const
{
    return m_model ? m_model->isStructureCompatible(value) : false;
}
//=============================================================================
QString
QtVariableTextEdit::getVariableClassAsString() const
{
    return m_model ? m_model->getVariableClassAsString() : QString();
}
//=============================================================================
QString
QtVariableTextEdit::getVariableDimensionsAsString() const
{
    return m_model ? m_model->getVariableDimensionsAsString() : QString();
}
//=============================================================================
QString
QtVariableTextEdit::variableName() const
{
    if (m_model) {
        return QString(m_model->variableName());
    }
    return QString();
}
//=============================================================================
const ArrayOf&
QtVariableTextEdit::array() const
{
    static const ArrayOf empty;
    return m_model ? m_model->array() : empty;
}
//=============================================================================
void
QtVariableTextEdit::showEvent(QShowEvent* event)
{
    QTextEdit::showEvent(event);
    if (m_model) {
        m_model->forceViewUpdate();
    }
}
//=============================================================================
void
QtVariableTextEdit::onModelContentChanged()
{
    if (m_model) {
        // Update the text content from the model
        QString newContent = m_model->getFormattedContent();

        // Preserve cursor position and scroll position
        QTextCursor cursor = textCursor();
        int cursorPosition = cursor.position();
        int scrollValue = verticalScrollBar()->value();

        setPlainText(newContent);

        // Restore cursor and scroll position
        int newLength = toPlainText().length();
        cursor.setPosition(qMin(cursorPosition, newLength));
        setTextCursor(cursor);
        verticalScrollBar()->setValue(scrollValue);
    }

    emit modelChanged();
}
//=============================================================================
void
QtVariableTextEdit::onModelError(const QString& message)
{
    emit errorOccurred(message);
}
//=============================================================================
void
QtVariableTextEdit::onModelVariableModified(const QString& variableName)
{
    emit variableModified(variableName);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
