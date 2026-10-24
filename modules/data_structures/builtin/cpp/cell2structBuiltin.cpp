//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "cell2structBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOfView.hpp"
#include "IsValidFieldname.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cell2structBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector ret;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 3);
    ArrayOf param1 = argIn[0];
    if (!param1.isCell()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"cell");
    }
    // field names: a cell or string array, or a character array (one per row)
    stringVector fieldnames;
    if (!(argIn[1].isCharacterArray() && argIn[1].isEmpty())) {
        fieldnames = argIn[1].asUtf8StringVector(false);
    }
    // dim: the dimension of the cell that runs along the fields (default 1)
    indexType dim = 0;
    if (argIn.size() == 3) {
        dim = argIn[2].asScalarIndex(false) - 1;
    }
    Dimensions cellDims = param1.dimensions();
    indexType cellRank = std::max(cellDims.length(), dim + 1);
    if (cellDims.dimensionLength(dim) != static_cast<indexType>(fieldnames.size())) {
        Error(L"Nelson:data_structures:fieldNamesMatchStructFields",
            L"Number of field names must match number of fields in new structure.");
    }
    for (const auto& fieldname : fieldnames) {
        if (!IsValidFieldname(fieldname)) {
            Error(L"Nelson:types:invalidFieldNames", L"Field names must be valid.");
        }
    }
    // the struct keeps the other dimensions of the cell (at least two)
    Dimensions structDims;
    indexType rank = 0;
    for (indexType d = 0; d < cellRank; ++d) {
        if (d != dim) {
            structDims[rank++] = cellDims.dimensionLength(d);
        }
    }
    if (rank < 2) {
        structDims[rank++] = 1;
    }
    indexType fieldCount = static_cast<indexType>(fieldnames.size());
    indexType structCount = structDims.elementCount();
    auto* qp = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, structCount, fieldnames, false));
    ArrayOf result(NLS_STRUCT_ARRAY, structDims, qp, false, fieldnames);
    if (structCount == 0 || fieldCount == 0) {
        ret << result;
        return ret;
    }
    // walk the cell: each element goes to the struct element addressed by the
    // other subscripts, in the field addressed by the subscript along dim.
    ArrayOfView<ArrayOf> cells(param1, ArrayOfViewAssumeValidated {});
    indexType cellCount = cellDims.elementCount();
    for (indexType k = 0; k < cellCount; ++k) {
        indexType remainder = k;
        indexType structIndex = 0;
        indexType stride = 1;
        indexType field = 0;
        for (indexType d = 0; d < cellRank; ++d) {
            indexType length = cellDims.dimensionLength(d);
            indexType subscript = remainder % length;
            remainder /= length;
            if (d == dim) {
                field = subscript;
            } else {
                structIndex += subscript * stride;
                stride *= length;
            }
        }
        qp[structIndex * fieldCount + field] = cells[k];
    }
    ret << result;
    return ret;
}
//=============================================================================
