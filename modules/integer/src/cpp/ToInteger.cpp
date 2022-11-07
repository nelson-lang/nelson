//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToInteger(NelsonType destinationClass, const ArrayOf& A)
{

    std::wstring destType = ClassToString(destinationClass);
    if (A.isSparse()) {
        Error(_W("Conversion to '") + destType + _W("' from sparse matrix is not possible."));
    }
    switch (A.getDataClass()) {
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        Error(_W("Invalid conversion from complex matrix to '") + destType + _W("' matrix."));
    } break;
    case NLS_GO_HANDLE: {
        Error(_W("Conversion to '") + destType + _W("' from graphic_object is not possible."));
    } break;
    case NLS_HANDLE: {
        Error(_W("Conversion to '") + destType + _W("' from handle is not possible."));
    } break;
    case NLS_STRING_ARRAY: {
        Error(_W("Conversion to '") + destType + _W("' from string is not possible."));
    } break;
    case NLS_CELL_ARRAY: {
        Error(_W("Conversion to '") + destType + _W("' from cell is not possible."));
    } break;
    case NLS_STRUCT_ARRAY: {
        if (A.getStructType() != "struct") {
            Error(_W("Undefined function '") + destType + _W("' for input arguments of type '")
                + utf8_to_wstring(A.getStructType()) + L"'.");
        } else {
            Error(_W("Conversion to '") + destType + _W("' from struct is not possible."));
        }
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
        Error(_W("Invalid conversion."));
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
