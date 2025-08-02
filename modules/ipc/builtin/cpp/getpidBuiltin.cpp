//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getpidBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "NelsonPIDs.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::IpcGateway::getpidBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    switch (argIn.size()) {
    case 0: {
        retval << ArrayOf::doubleConstructor((double)getCurrentPID());
    } break;
    case 1: {
        std::wstring param = argIn[0].getContentAsWideString();
        if (param == L"available") {
            std::vector<int> pids = getNelsonPIDs();
            ArrayOf res;
            Dimensions dims(1, pids.size());
            double* pd = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, (indexType)pids.size());
            res = ArrayOf(NLS_DOUBLE, dims, pd);
            for (indexType k = 0; k < dims.getElementCount(); ++k) {
                pd[k] = (double)pids[k];
            }
            retval << res;
        } else {
            Error(_("Wrong value for #1 argument: 'available' expected."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
