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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
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
    if (!A.isHandle() && !B.isHandle()) {
        Error(L"Nelson:handle:handleExpected", L"handle expected.");
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        RaiseError(
            std::string(resolveMessageTemplate("Nelson:graphics:sizeMismatchArgumentsArithmetic",
                "Size mismatch on arguments to arithmetic operator"))
                + " " + EQ_OPERATOR_STR,
            "Nelson:dimagree");
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
    Clen = Cdim.elementCount();
    // initialized to false: a handle is never equal to a non-handle value.
    void* Cp = new_with_exception<logical>(Clen, true);
    auto* C = static_cast<logical*>(Cp);
    if (A.isHandle() && B.isHandle()) {
        auto* hA = (HandleRef*)A.dataPointer();
        auto* hB = (HandleRef*)B.dataPointer();
        if ((Astride == 1) && (Bstride == 1)) {
            for (indexType i = 0; i < Clen; i++) {
                C[i] = (hA[i] && hB[i] && hA[i]->id() == hB[i]->id()) ? 1 : 0;
            }
        } else {
            if (Astride != 0) {
                for (indexType i = 0; i < Clen; i++) {
                    C[i] = (hA[i] && hB[0] && hA[i]->id() == hB[0]->id()) ? 1 : 0;
                }
            } else {
                for (indexType i = 0; i < Clen; i++) {
                    C[i] = (hA[0] && hB[i] && hA[0]->id() == hB[i]->id()) ? 1 : 0;
                }
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
