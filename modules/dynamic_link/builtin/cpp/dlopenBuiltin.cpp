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
#include "NelsonException.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
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
        std::wstring libraryPath = param1.asWideString();
        DynamicLinkLibraryObject* dlObject = nullptr;
        try {
            dlObject = new DynamicLinkLibraryObject(libraryPath);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        } catch (const NelsonException&) {
            throw;
        }
        retval << ArrayOf::handle(dlObject);
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    return retval;
}
//=============================================================================
