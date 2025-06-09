//=============================================================================
#pragma once
//=============================================================================
#include "VariableDefaultModel.h"
#include <QtWidgets/QTextEdit>
#include <QtCore/QPointer>
//=============================================================================
namespace Nelson {
//=============================================================================
class QtVariableTextEdit : public QTextEdit
{
    Q_OBJECT
    //=============================================================================
public:
    explicit QtVariableTextEdit(const QString& variableName, ArrayOf array, Evaluator* evaluator,
        QWidget* parent = nullptr);
    ~QtVariableTextEdit() override = default;

    // Model access - similar to QTableView::model()
    VariableDefaultModel*
    model() const
    {
        return m_model;
    }

    // Convenience methods that delegate to the model
    void
    refreshFromArray(const ArrayOf& value);
    bool
    isStructureCompatible(const ArrayOf& value) const;
    QString
    getVariableClassAsString() const;
    QString
    getVariableDimensionsAsString() const;

    // Direct access to variable data
    QString
    variableName() const;
    const ArrayOf&
    array() const;
    //=============================================================================
signals:
    void
    modelChanged();
    void
    errorOccurred(const QString& message);
    void
    variableModified(const QString& variableName);
    //=============================================================================
protected:
    // Override to handle model updates
    void
    showEvent(QShowEvent* event) override;
    //=============================================================================
private slots:
    void
    onModelContentChanged();
    void
    onModelError(const QString& message);
    void
    onModelVariableModified(const QString& variableName);
    //=============================================================================
private:
    QPointer<VariableDefaultModel> m_model;

    void
    setupConnections();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
