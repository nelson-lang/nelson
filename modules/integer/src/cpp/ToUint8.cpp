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
#include "ToUint8.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToUint8(ArrayOf a)
{
    std::string destType = "uint8";
    Class destClass = NLS_UINT8;
    if (a.isSparse()) {
        throw Exception(
            _("Conversion to '") + destType + _("' from sparse matrix is not possible."));
    }
    switch (a.getDataClass()) {
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        throw Exception(
            _("Invalid conversion from complex matrix to '") + destType + _("' matrix."));
    } break;
    case NLS_HANDLE: {
        throw Exception(_("Conversion to '") + destType + _("' from handle is not possible."));
    } break;
    case NLS_CELL_ARRAY: {
        throw Exception(_("Conversion to '") + destType + _("' from cell is not possible."));
    } break;
    case NLS_STRUCT_ARRAY: {
        if (a.getStructType() != "struct") {
            throw Exception(_("Undefined function '") + destType
                + _("' for input arguments of type '") + a.getStructType() + "'.");
        } else {
            throw Exception(_("Conversion to '") + destType + _("' from struct is not possible."));
        }
    } break;
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_CHAR: {
        ArrayOf res(a);
        res.promoteType(NLS_UINT8);
        return res;
    } break;
    default: {
        throw Exception(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
