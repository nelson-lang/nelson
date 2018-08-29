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
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isLogical() const
{
    return (dp->dataClass == NLS_LOGICAL);
}
//=============================================================================
bool
ArrayOf::isNdArrayLogical() const
{
    return (dp->dataClass == NLS_LOGICAL) && !is2D();
}
//=============================================================================
bool
ArrayOf::isSparseLogicalType() const
{
    return (dp->dataClass == NLS_LOGICAL) && (dp->sparse) && is2D();
}
//=============================================================================
ArrayOf
ArrayOf::logicalConstructor(bool aval)
{
    Dimensions dim;
    dim.makeScalar();
    logical* data = (logical*)allocateArrayOf(NLS_LOGICAL, 1);
    *data = (logical)aval;
    return ArrayOf(NLS_LOGICAL, dim, data);
}
//=============================================================================
logical
ArrayOf::getContentAsLogicalScalar(bool arrayAsScalar) const
{
    if (!isLogical()) {
        Error(ERROR_TYPE_LOGICAL_EXPECTED);
    }
    if (isEmpty() || (!arrayAsScalar && !isScalar())) {
        Error(ERROR_SIZE_SCALAR_EXPECTED);
    }
    logical* qp = (logical*)dp->getData();
    return (*qp);
}
}
//=============================================================================
