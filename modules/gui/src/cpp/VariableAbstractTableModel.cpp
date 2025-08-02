//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableAbstractTableModel.h"
//=============================================================================
namespace Nelson {
//=============================================================================
QString
VariableAbstractTableModel::variableName() const
{
    return m_variableName;
}
//=============================================================================
const ArrayOf&
VariableAbstractTableModel::array() const
{
    return m_array;
}
//=============================================================================
NelsonType
VariableAbstractTableModel::getDataClass()
{
    return m_array.getDataClass();
}
//=============================================================================
Evaluator*
VariableAbstractTableModel::getEvaluator()
{
    return m_evaluator;
}
//=============================================================================
QString
VariableAbstractTableModel::getVariableClassAsString() const
{
    std::wstring classname;
    ClassName(m_array, classname);
    return wstringToQString(classname);
}
//=============================================================================
QString
VariableAbstractTableModel::getVariableDimensionsAsString() const
{
    Dimensions dims = m_array.getDimensions();
    return wstringToQString(dims.toWideString());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
