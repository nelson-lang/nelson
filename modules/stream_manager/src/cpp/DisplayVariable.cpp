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
#include "DisplayVariable.hpp"
#include "DisplayCharacters.hpp"
#include "DisplayStringArray.hpp"
#include "DisplayDouble.hpp"
#include "DisplaySingle.hpp"
#include "DisplayLogical.hpp"
#include "DisplayIntegers.hpp"
#include "DisplayCell.hpp"
#include "DisplayStruct.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
DisplayVariable(Interface* io, const ArrayOf& A, bool fromDispBuiltin, bool& needToOverload)
{
    needToOverload = false;
    if (io == nullptr) {
        return;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return;
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY: {
        DisplayCell(io, A, fromDispBuiltin, needToOverload);
    } break;
    case NLS_STRING_ARRAY: {
        DisplayStringArray(io, A, fromDispBuiltin, needToOverload);
    } break;
    case NLS_STRUCT_ARRAY: {
        DisplayStruct(io, A, fromDispBuiltin, needToOverload);
    } break;
    case NLS_CHAR: {
        DisplayCharacters(io, A, fromDispBuiltin, needToOverload);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        DisplayDouble(io, A, fromDispBuiltin, needToOverload);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        DisplaySingle(io, A, fromDispBuiltin, needToOverload);
    } break;
    case NLS_LOGICAL: {
        DisplayLogical(io, A, fromDispBuiltin, needToOverload);
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        DisplayInteger(io, A, fromDispBuiltin, needToOverload);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
