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
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_CELL_EXPECTED);
    }
    ArrayOf param2 = argIn[1];
    stringVector fieldnames = param2.getContentAsCStringVector(false);
    indexType dim = 0;
    if (argIn.size() == 2) {
        dim = 0;
    } else {
        ArrayOf param3 = argIn[2];
        dim = param3.getContentAsScalarIndex(false) - 1;
    }
    if (dim > 1) {
        Error(_W("Not yet implemented with dim > 2"));
    }
    Dimensions dims1 = param1.getDimensions();
    //    if (fieldnames.size() != 1)
    if (!param1.isEmpty()) {
        if (dims1[dim] != fieldnames.size()) {
            Error(_W("Number of field names must match number of fields in new structure."));
        }
    }
    auto* arg = (ArrayOf*)(param1.getDataPointer());
    if (dim == 0) {
        if (param1.isEmpty()) {
            Dimensions dims2 = param2.getDimensions();
            indexType len = std::min(dims1.getLength(), dims2.getLength());
            Dimensions dims;
            if (dims1.equals(dims2)) {
                indexType l = 0;
                for (indexType k = dim + 1; k < dims1.getLength(); k++) {
                    dims[l] = dims1[k];
                    l++;
                }
                if (dims.getLength() < len) {
                    dims[dims.getLength()] = 1;
                }
                dims.simplify();
            } else if (dims1.getLength() > dims2.getLength()) {
                indexType l = 0;
                for (indexType k = dim + 1; k < dims1.getLength(); k++) {
                    dims[l] = dims1[k];
                    l++;
                }
            } else {
                if (dims1[dim] == 0) {
                    dims[0] = 1;
                } else {
                    dims[0] = dims1.getElementCount() / dims1[dim];
                }
                dims[1] = 1;
            }
            if (dims.isScalar()) {
                ret << ArrayOf::emptyStructWithoutFields();
            } else if (dims.getElementCount() == 0) {
                ArrayOf c = ArrayOf::emptyConstructor(dims);
                c.promoteType(NLS_STRUCT_ARRAY);
                ret << c;
            } else {
                ret << ArrayOf::emptyStructConstructor(fieldnames, dims);
            }
        } else {
            Dimensions dims;
            dims[1] = 1;
            dims[0] = dims1.getElementCount() / dims1[dim];
            auto* qp = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
            ArrayOf c = ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fieldnames);
            ompIndexType elementCount = param1.getElementCount();
            for (ompIndexType k = 0; k < elementCount; k++) {
                qp[k] = arg[k];
            }
            ret << c;
        }
    } else /* dim == 1 */
    {
        Dimensions dims;
        dims[0] = dims1.getElementCount() / dims1[dim];
        dims[1] = 1;
        auto* qp = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
        size_t l = 0;
        indexType rowCount = param1.getDimensions()[0];
        indexType colCount = param1.getDimensions()[1];
        for (indexType i = 0; i < rowCount; i++) {
            for (indexType j = 0; j < colCount; j++) {
                qp[l] = arg[i + j * rowCount];
                l++;
            }
        }
        ret << ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fieldnames);
    }
    return ret;
}
//=============================================================================
