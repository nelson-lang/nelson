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
#include "xorBuiltin.hpp"
#include "XorLogical.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
checkNoNaN(const ArrayOf& value)
{
    if (value.isSparse()) {
        return;
    }
    if (value.dataClass() == NLS_DOUBLE) {
        const auto* ptr = static_cast<const double*>(value.dataPointer());
        indexType elementCount = value.elementCount();
        for (indexType k = 0; k < elementCount; ++k) {
            if (std::isnan(ptr[k])) {
                Error(L"Nelson:logical:nanNotAllowed",
                    L"Conversion to logical with NaN is not possible.");
            }
        }
    } else if (value.dataClass() == NLS_SINGLE) {
        const auto* ptr = static_cast<const single*>(value.dataPointer());
        indexType elementCount = value.elementCount();
        for (indexType k = 0; k < elementCount; ++k) {
            if (std::isnan(ptr[k])) {
                Error(L"Nelson:logical:nanNotAllowed",
                    L"Conversion to logical with NaN is not possible.");
            }
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::LogicalGateway::xorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    for (int k = 0; k < 2; ++k) {
        if (argIn[k].isSparse() || argIn[k].isCell() || argIn[k].isHandle() || argIn[k].isStruct()
            || argIn[k].isClassType()) {
            OverloadRequired("xor");
        }
    }
    checkNoNaN(argIn[0]);
    checkNoNaN(argIn[1]);
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    A.promoteType(NLS_LOGICAL);
    B.promoteType(NLS_LOGICAL);
    retval << XorLogical(A, B);
    return retval;
}
//=============================================================================
