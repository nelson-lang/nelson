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
#include "DisplayCell.hpp"
#include "DisplayStruct.hpp"
#include "DisplayString.hpp"
#include "DisplayChar.hpp"
#include "DisplayLogical.hpp"
#include "DisplayInteger.hpp"
#include "DisplayDouble.hpp"
#include "DisplayDoubleComplex.hpp"
#include "DisplaySparseDouble.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
DisplayVariable(Interface* io, const ArrayOf& A, const std::wstring& name, bool& needToOverload)
{
    if (io == nullptr) {
        return;
    }
    switch (A.getDataClass()) {
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        DisplayInteger(io, A, name);
        needToOverload = false;
    } break;
    case NLS_LOGICAL: {
        DisplayLogical(io, A, name);
        needToOverload = false;
    } break;
    case NLS_CHAR: {
        DisplayChar(io, A, name);
        needToOverload = false;
    } break;
    case NLS_STRING_ARRAY: {
        DisplayString(io, A, name);
        needToOverload = false;
    } break;
    case NLS_STRUCT_ARRAY: {
        DisplayStruct(io, A, name);
        needToOverload = false;
    } break;
    case NLS_CELL_ARRAY: {
        DisplayCell(io, A, name);
        needToOverload = false;
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            DisplaySparseDouble(io, A, name);
        } else {
            DisplayDouble(io, A, name);
        }
        needToOverload = false;
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {

        } else {
            DisplayDoubleComplex(io, A, name);
        }
        needToOverload = false;
    } break;
    default: {
        needToOverload = true;
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
