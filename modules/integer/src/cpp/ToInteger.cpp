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
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToInteger(NelsonType destinationClass, const ArrayOf& A)
{

    std::wstring destType = ClassToStringW(destinationClass);
    if (A.isSparse()) {
        Error(L"Nelson:integer:cannotConvertFromSparse",
            L"Conversion to '%s' from sparse matrix is not possible.", destType);
    }
    switch (A.dataClass()) {
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        Error(L"Nelson:integer:cannotConvertFromComplex",
            L"Invalid conversion from complex matrix to '%s' matrix.", destType);
    } break;
    case NLS_GO_HANDLE: {
        Error(L"Nelson:integer:cannotConvert:conversionValueFromGraphicsObjectNot",
            L"Conversion to '%s' from graphics_object is not possible.", destType);
    } break;
    case NLS_HANDLE: {
        Error(L"Nelson:integer:cannotConvert:conversionValueFromHandleNotPossible",
            L"Conversion to '%s' from handle is not possible.", destType);
    } break;
    case NLS_STRING_ARRAY: {
        Error(L"Nelson:integer:cannotConvert:conversionValueFromStringNotPossible",
            L"Conversion to '%s' from string is not possible.", destType);
    } break;
    case NLS_CELL_ARRAY: {
        Error(L"Nelson:integer:cannotConvert", L"Conversion to '%s' from cell is not possible.",
            destType);
    } break;
    case NLS_FUNCTION_HANDLE: {
        Error(L"Nelson:integer:cannotConvert:conversionValueFromFunctionHandleNot",
            L"Conversion to '%s' from function_handle is not possible.", destType);
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        Error(L"Nelson:integer:conversionNotDefined",
            L"Undefined function '%s' for input arguments of type '%s'.", destType,
            utf8_to_wstring(A.classType()));
    } break;
    case NLS_STRUCT_ARRAY: {
        Error(L"Nelson:integer:cannotConvert:conversionValueFromStructNotPossible",
            L"Conversion to '%s' from struct is not possible.", destType);
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
        Error(L"Nelson:integer:invalidConversion", L"Invalid conversion.");
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
