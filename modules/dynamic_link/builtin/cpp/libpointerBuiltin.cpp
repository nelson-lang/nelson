//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "libpointerBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "LibPointerObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    LibPointerObject* libPointerObject = nullptr;
    ArrayOf Value;
    switch (argIn.size()) {
    case 0: {
        try {
            libPointerObject = new LibPointerObject();
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        } catch (const NelsonException&) {
            throw;
        }
    } break;
    case 1: {
        std::wstring DataType = argIn[0].asWideString();
        try {
            libPointerObject = new LibPointerObject(DataType);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        } catch (const NelsonException&) {
            throw;
        }
    } break;
    case 2: {
        std::wstring DataType = argIn[0].asWideString();
        ArrayOf Value = argIn[1];
        try {
            libPointerObject = new LibPointerObject(DataType, Value);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        } catch (const NelsonException&) {
            throw;
        }
    } break;
    default:
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
        break;
    }
    retval << ArrayOf::handle(libPointerObject);
    return retval;
}
//=============================================================================
