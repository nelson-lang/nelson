//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToInteger.hpp"
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToInteger(NelsonType destinationClass, const ArrayOf& A)
{

    std::wstring destType = ClassToStringW(destinationClass);
    if (A.isSparse()) {
        raiseError(L"Nelson:integer:ERROR_CONVERSION_FROM_SPARSE_NOT_POSSIBLE",
            ERROR_CONVERSION_FROM_SPARSE_NOT_POSSIBLE, destType);
    }
    switch (A.getDataClass()) {
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        raiseError(L"Nelson:integer:ERROR_INVALID_CONVERSION_FROM_COMPLEX",
            ERROR_INVALID_CONVERSION_FROM_COMPLEX, destType);
    } break;
    case NLS_GO_HANDLE: {
        raiseError(L"Nelson:integer:ERROR_CONVERSION_FROM_GRAPHICS_OBJECT_NOT_POSSIBLE",
            ERROR_CONVERSION_FROM_GRAPHICS_OBJECT_NOT_POSSIBLE, destType);
    } break;
    case NLS_HANDLE: {
        raiseError(L"Nelson:integer:ERROR_CONVERSION_FROM_HANDLE_NOT_POSSIBLE",
            ERROR_CONVERSION_FROM_HANDLE_NOT_POSSIBLE, destType);
    } break;
    case NLS_STRING_ARRAY: {
        raiseError(L"Nelson:integer:ERROR_CONVERSION_FROM_STRING_NOT_POSSIBLE",
            ERROR_CONVERSION_FROM_STRING_NOT_POSSIBLE, destType);
    } break;
    case NLS_CELL_ARRAY: {
        raiseError(L"Nelson:integer:ERROR_CONVERSION_FROM_CELL_NOT_POSSIBLE",
            ERROR_CONVERSION_FROM_CELL_NOT_POSSIBLE, destType);
    } break;
    case NLS_FUNCTION_HANDLE: {
        raiseError(L"Nelson:integer:ERROR_CONVERSION_FROM_FUNCTION_HANDLE_NOT_POSSIBLE",
            ERROR_CONVERSION_FROM_FUNCTION_HANDLE_NOT_POSSIBLE, destType);
    } break;
    case NLS_CLASS_ARRAY: {
        raiseError(L"Nelson:integer:ERROR_UNDEFINED_FUNCTION_FOR_INPUT_TYPE",
            ERROR_UNDEFINED_FUNCTION_FOR_INPUT_ARGUMENTS, destType,
            utf8_to_wstring(A.getClassType()));
    } break;
    case NLS_STRUCT_ARRAY: {
        raiseError(L"Nelson:integer:ERROR_CONVERSION_FROM_STRUCT_NOT_POSSIBLE",
            ERROR_CONVERSION_FROM_STRUCT_NOT_POSSIBLE, destType);
    } break;
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_CHAR:
    case NLS_SINGLE:
    case NLS_DOUBLE: {
        ArrayOf res(A);
        res.promoteType(destinationClass);
        return res;
    } break;
    default: {
        raiseError(L"Nelson:integer:ERROR_INVALID_CONVERSION", ERROR_INVALID_CONVERSION);
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
