//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "iskeywordBuiltin.hpp"
#include "Error.hpp"
#include "Keywords.hpp"
#include "NewWithException.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::iskeywordBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 0) {
        wstringVector keys = GetKeywords(true);
        ArrayOf* elements = new_with_exception<ArrayOf>(keys.size(), false);
        for (size_t k = 0; k < keys.size(); k++) {
            elements[k] = ArrayOf::characterArrayConstructor(keys[k]);
        }
        Dimensions dims(keys.size(), 1);
        retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring warg = argIn[0].getContentAsWideString();
        retval << ArrayOf::logicalConstructor(isKeyword(warg));
    }
    return retval;
}
//=============================================================================
