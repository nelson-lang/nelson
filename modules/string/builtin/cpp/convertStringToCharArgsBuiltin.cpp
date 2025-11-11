//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Error.hpp"
#include "i18n.hpp"
#include "convertStringToCharArgsBuiltin.hpp"
#include "ConvertStringsToChars.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::convertStringToCharArgsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    if (argIn[0].isScalarStringArray()) {
        retval << ConvertStringsToChars(argIn[0], false);
    } else if (argIn[0].isCell()) {
        ArrayOf* argsCells = (ArrayOf*)argIn[0].getDataPointer();
        ArrayOf* modifiedCells
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, argIn[0].getElementCount());
        for (size_t k = 0; k < argIn[0].getElementCount(); k++) {
            modifiedCells[k] = ConvertStringsToChars(argsCells[k], false);
        }
        ArrayOf convertedCell = ArrayOf(NLS_CELL_ARRAY, argIn[0].getDimensions(), modifiedCells);
        retval << convertedCell;
    } else {
        retval << argIn[0];
    }
    return retval;
}
//=============================================================================
