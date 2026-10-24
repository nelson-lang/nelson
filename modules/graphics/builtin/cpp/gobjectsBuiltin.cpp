//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <vector>
#include "gobjectsBuiltin.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static indexType
checkedDimension(double value)
{
    if (value < 0 || std::floor(value) != value) {
        Error(L"Nelson:gobjects:invalidDimensions", L"Dimensions must be nonnegative integers.");
    }
    return static_cast<indexType>(value);
}
//=============================================================================
static indexType
dimensionFromArray(const ArrayOf& arg)
{
    return checkedDimension(arg.asDoubleScalar());
}
//=============================================================================
// Build a Dimensions from a single size-vector argument (gobjects(sz)), like
// zeros(sz) / ones(sz): each element is a dimension, and a 1-element vector
// (including a scalar) gives a square n-by-n result.
static Dimensions
dimensionsFromSizeVector(const ArrayOf& arg)
{
    ArrayOf dbl(arg);
    dbl.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(dbl.dataPointer());
    ompIndexType elementCount = static_cast<ompIndexType>(dbl.elementCount());
    std::vector<indexType> values;
    values.reserve(elementCount);
    for (ompIndexType k = 0; k < elementCount; k++) {
        values.push_back(checkedDimension(ptr[k]));
    }
    Dimensions dims(values);
    if (dims.length() == 1) {
        dims[1] = dims[0];
    }
    return dims;
}
//=============================================================================
ArrayOfVector
gobjectsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    Dimensions dims(1, 1);
    if (argIn.size() == 1) {
        const ArrayOf& arg = argIn[0];
        if (arg.isEmpty()) {
            // an empty size argument gives an empty array, like zeros([])
            dims = Dimensions(0, 0);
        } else if (arg.isRowVector()) {
            dims = dimensionsFromSizeVector(arg);
        } else {
            Error(
                L"Nelson:gobjects:invalidDimensions", L"Dimensions must be nonnegative integers.");
        }
    } else if (!argIn.empty()) {
        std::vector<indexType> values;
        values.reserve(argIn.size());
        for (const auto& arg : argIn) {
            values.push_back(dimensionFromArray(arg));
        }
        dims = Dimensions(values);
    }

    std::vector<go_handle> handles(dims.elementCount(), HANDLE_UNASSIGNED_OBJECT);

    ArrayOfVector retval;
    retval << ArrayOf::graphicsObject(handles, dims, GO_CLASS_NAME_GRAPHICS_PLACEHOLDER_STR);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
