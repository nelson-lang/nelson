//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ToInteger.hpp"
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToInteger(Class destinationClass, const ArrayOf& A)
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
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
