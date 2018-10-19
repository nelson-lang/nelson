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
#include "SparseDynamicFunctions.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isSparse() const
{
    if (dp == nullptr) {
        return false;
    }
    return (dp->sparse);
}
//=============================================================================
bool
ArrayOf::isSparseDoubleType(bool realOnly) const
{
    if (dp->sparse) {
        return dp->dataClass == NLS_DOUBLE || (dp->dataClass == NLS_DCOMPLEX && !realOnly);
    }
    return false;
}
//=============================================================================
const void*
ArrayOf::getSparseDataPointer() const
{
    if (dp) {
        return dp->getData();
    } else {
        return nullptr;
    }
}
//=============================================================================
void
ArrayOf::makeDense()
{
    if (!isSparse()) {
        return;
    }
    if (isEmpty()) {
        dp->sparse = false;
        return;
    }
    ensureSingleOwner();
    dp = dp->putData(dp->dataClass, dp->dimensions,
        MakeDenseArrayOfDynamicFunction(
            dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData()),
        false, dp->fieldNames);
}
//=============================================================================
indexType
ArrayOf::getNonzeros() const
{
    if (!isSparse()) {
        return (dp->dimensions.getElementCount());
    }
    if (isEmpty()) {
        return 0;
    }
    return CountNonzerosDynamicFunction(
        dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData());
}
//=============================================================================
void
ArrayOf::makeSparse()
{
    if (!is2D()) {
        Error(_W("Cannot make n-dimensional arrays sparse."));
    }
    if (isEmpty()) {
        dp = dp->putData(dp->dataClass, dp->dimensions, NULL, true, dp->fieldNames);
        return;
    }
    if (isReferenceType() || isCharacterArray()) {
        Error(_W("Cannot make strings or reference types sparse."));
    }
    if (isSparse()) {
        return;
    }
    if ((dp->dataClass == NLS_DOUBLE) || (dp->dataClass == NLS_DCOMPLEX)
        || (dp->dataClass == NLS_LOGICAL)) {
        ensureSingleOwner();
    } else {
        Error(_W("Cannot make sparse."));
    }
    dp = dp->putData(dp->dataClass, dp->dimensions,
        MakeSparseArrayOfDynamicFunction(
            dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData()),
        true, dp->fieldNames);
}
//=============================================================================

}
//=============================================================================