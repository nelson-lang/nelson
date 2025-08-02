//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "COM_rangeBuiltin.hpp"
#include "ComExcelHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_rangeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring range = param1.getContentAsWideString();
        retval << ArrayOf::logicalConstructor(isValidRange(range));
    } else {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        indexType m = param1.getContentAsScalarIndex();
        indexType n = param2.getContentAsScalarIndex();
        retval << ArrayOf::characterArrayConstructor(xlsIndexToRange(m, n));
    }
    return retval;
}
//=============================================================================
