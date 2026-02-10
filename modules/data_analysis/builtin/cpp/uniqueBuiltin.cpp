//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uniqueBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "Unique.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Warning.hpp"
//=============================================================================
using namespace Nelson;
ArrayOfVector
Nelson::DataAnalysisGateway::uniqueBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // C = unique(A)
    // C = unique(A,'rows')
    //[C,ia,ic] = unique(...)

    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 3);

    ArrayOf input(argIn[0]);
    bool asRows = false;

    if (argIn.size() == 2) {
        if (!(argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray())) {
            raiseError(L"Nelson:data_analysis:ERROR_UNIQUE_SECOND_ARGUMENT_MUST_BE_ROWS",
                ERROR_UNIQUE_SECOND_ARGUMENT_MUST_BE_ROWS);
        }
        std::wstring direction = argIn[1].getContentAsWideString();
        if (direction == L"rows") {
            asRows = true;
        } else {
            raiseError(L"Nelson:data_analysis:ERROR_UNIQUE_SECOND_ARGUMENT_MUST_BE_ROWS",
                ERROR_UNIQUE_SECOND_ARGUMENT_MUST_BE_ROWS);
        }
    }
    Dimensions dims(input.getDimensions());
    if (asRows && (!dims.is2D())) {
        raiseError(L"Nelson:data_analysis:ERROR_ROWS_MODE_ONLY_WORKS_FOR_2D_MATRIX",
            ERROR_ROWS_MODE_ONLY_WORKS_FOR_2D_MATRIX);
    }

    bool needToOverload = false;
    if (input.isCell() && asRows) {
        Warning(_W("'rows' option is not supported for cell array inputs."));
        asRows = false;
    }
    retval = Unique(input, nLhs, asRows, needToOverload);
    if (needToOverload) {
        OverloadRequired("unique");
    }
    return retval;
}
//=============================================================================
