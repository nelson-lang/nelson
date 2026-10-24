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
#include "Hypotenuse.hpp"
#include "MatrixCheck.hpp"
#include "HypotenuseReal.hpp"
#include "HypotenuseComplex.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Hypotenuse(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar()) {
            res = B;
        } else if (B.isScalar()) {
            res = A;
        } else if (A.dimensions().equals(B.dimensions())) {
            // Two empty operands of the same shape give an empty result of that shape.
            res = A;
        } else {
            Error(L"Nelson:sizeDimensionsMustMatch",
                L"Arrays have incompatible sizes for this operation.");
        }
        if (res.dataClass() == NLS_DCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
        if (res.dataClass() == NLS_SCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_SINGLE);
            }
        }
        return res;
    }
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return {};
    }

    switch (A.dataClass()) {
    case NLS_SINGLE: {
        res = real_hypotenuse<single>(NLS_SINGLE, A, B);
    } break;
    case NLS_DOUBLE: {
        res = real_hypotenuse<double>(NLS_DOUBLE, A, B);
    } break;
    case NLS_SCOMPLEX: {
        res = complex_hypotenuse<single>(NLS_SCOMPLEX, A, B);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        res = complex_hypotenuse<double>(NLS_DCOMPLEX, A, B);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
