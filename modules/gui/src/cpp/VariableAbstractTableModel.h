//=============================================================================
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include <QtCore/QAbstractTableModel>
#include <QtCore/QString>
//=============================================================================
namespace Nelson {

class VariableAbstractTableModel : public QAbstractTableModel
{
    //=============================================================================
    Q_OBJECT
    //=============================================================================
public:
    VariableAbstractTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr)
        : m_variableName(name)
        , m_array(array)
        , m_evaluator(evaluator)
        , QAbstractTableModel(parent) {};
    //=============================================================================
    ~VariableAbstractTableModel() override = default;
    //=============================================================================
    QString
    variableName() const
    {
        return m_variableName;
    }
    //=============================================================================
    const ArrayOf&
    array() const
    {
        return m_array;
    }
    //=============================================================================
    NelsonType
    getDataClass()
    {
        return m_array.getDataClass();
    }
    //=============================================================================
    Evaluator*
    getEvaluator()
    {
        return m_evaluator;
    }
    //=============================================================================
protected:
    ArrayOf m_array;
    QString m_variableName;
    Evaluator* m_evaluator;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
