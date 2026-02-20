//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "EqHandle.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "MatrixCheck.hpp"
#include "NewWithException.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
EqHandle(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    if (!A.isHandle()) {
        raiseError2(_E("nelson:validators:mustBeHandle"));
    }
    if (!A.isHandle() && !B.isHandle()) {
        raiseError2(_E("nelson:validators:mustBeHandle"));
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        raiseError(L"Nelson:handle:ERROR_SIZE_MISMATCH_ON_OPERATOR",
            ERROR_SIZE_MISMATCH_ON_OPERATOR, utf8_to_wstring(EQ_OPERATOR_STR));
    }
    int Astride = 0;
    int Bstride = 0;
    indexType Clen = 0;
    Dimensions Cdim;
    if (A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = dimsB;
    } else if (B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = dimsA;
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = dimsA;
    }
    Clen = Cdim.getElementCount();
    void* Cp = new_with_exception<logical>(Clen, false);
    auto* C = static_cast<logical*>(Cp);
    if (A.isHandle() && B.isHandle()) {
        auto* hA = (nelson_handle*)A.getDataPointer();
        auto* hB = (nelson_handle*)B.getDataPointer();
        if ((Astride == 1) && (Bstride == 1)) {
            for (indexType i = 0; i < Clen; i++) {
                C[i] = (hA[i] == hB[i]) ? 1 : 0;
            }
        } else {
            if (Astride != 0) {
                for (indexType i = 0; i < Clen; i++) {
                    C[i] = (hA[i] == hB[0]) ? 1 : 0;
                }
            } else {
                for (indexType i = 0; i < Clen; i++) {
                    C[i] = (hA[0] == hB[i]) ? 1 : 0;
                }
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
