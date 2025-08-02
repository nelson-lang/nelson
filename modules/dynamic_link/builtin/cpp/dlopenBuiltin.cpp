//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlopenBuiltin.hpp"
#include "Exception.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlopenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isRowVectorCharacterArray()) {
        ArrayOf param1 = argIn[0];
        std::wstring libraryPath = param1.getContentAsWideString();
        DynamicLinkLibraryObject* dlObject = nullptr;
        try {
            dlObject = new DynamicLinkLibraryObject(libraryPath);
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        } catch (const Exception&) {
            throw;
        }
        retval << ArrayOf::handleConstructor(dlObject);
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return retval;
}
//=============================================================================
