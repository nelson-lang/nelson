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
#include <Eigen/Dense>
#include "UnaryPlus.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
UnaryPlus(ArrayOf& A, bool mustRaiseError, bool& bSuccess)
{
    bSuccess = false;
    if (A.isSparse()) {
        if (mustRaiseError) {
            std::string overload = ClassName(A) + "_uplus";
            throw Exception(_("function") + " " + overload + " " + _("undefined."));
        } else {
            return ArrayOf();
        }
    }
    ArrayOf res;
    switch (A.getDataClass()) {
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = A;
        res.ensureSingleOwner();
        res.promoteType(NLS_DOUBLE);
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX: {
        res = A;
        res.ensureSingleOwner();
    } break;
    default: {
        if (mustRaiseError) {
            std::string overload = ClassName(A) + "_uplus";
            throw Exception(_("function") + " " + overload + " " + _("undefined."));
        } else {
            bSuccess = false;
            return ArrayOf();
        }
    } break;
    }
    bSuccess = true;
    return res;
}
}
//=============================================================================
