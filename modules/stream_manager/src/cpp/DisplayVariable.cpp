//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "DisplayVariable.hpp"
#include "DisplayVariableHelpers.hpp"
#include "DisplayFloatingNumber.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
canDisplayWithoutOverload(const ArrayOf& A)
{

    if (A.isSparse() || A.isClassStruct()) {
        return false;
    }
    Class variableClass = A.getDataClass();
    bool withoutOverload;
    switch (variableClass) {
    case NLS_DCOMPLEX:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_SINGLE:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_LOGICAL:
    case NLS_CHAR:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        withoutOverload = true;
    } break;
    default: {
        withoutOverload = false;
    } break;
    }
    return withoutOverload;
}
//=============================================================================
void
DisplayVariable(Interface* io, const ArrayOf& A, const std::string& name, bool& needToOverload)
{
    if (io == nullptr) {
        return;
    }
    if (canDisplayWithoutOverload(A)) {
        std::wstring wname = utf8_to_wstring(name);
        DisplayVariableHeader(io, A, wname);
        DisplayVariableValue(io, A, wname);
        DisplayVariableFooter(io, A, wname);
        needToOverload = false;
    } else {
        needToOverload = true;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
