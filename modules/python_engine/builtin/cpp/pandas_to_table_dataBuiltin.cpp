//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pandas_to_table_dataBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonObjectHandle.hpp"
#include "Types.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// [tf, columns, variableNames, rowNames] = __pandas_to_table_data__(pyObject)
//
// Internal helper used by the @table constructor to turn a pandas DataFrame or
// Series into the pieces needed to build a Nelson table. Returns tf = false
// (with empty trailing outputs) for anything that is not a pandas DataFrame or
// Series, so the caller can fall through to the normal table construction.
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::pandas_to_table_dataBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 4);

    ArrayOf columns;
    ArrayOf variableNames;
    ArrayOf rowNames;
    bool converted = false;

    const ArrayOf& arg = argIn[0];
    if (arg.isHandle() && arg.isScalar()
        && arg.handleCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        HandleGenericObject* hgo = arg.asHandleScalar();
        if (hgo) {
            PythonObjectHandle* poh = (PythonObjectHandle*)hgo;
            converted = poh->toTableData(columns, variableNames, rowNames);
        }
    }

    retval << ArrayOf::logicalScalar(converted);
    retval << columns;
    retval << variableNames;
    retval << rowNames;
    return retval;
}
//=============================================================================
