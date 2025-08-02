//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "StringFind.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
StringFind(const std::wstring& A, const std::wstring& B)
{
    ArrayOf res;
    std::vector<double> vectorRes;
    std::string::size_type found;
    if (!B.empty()) {
        size_t start = 0;
        while (true) {
            found = A.find(B, start);
            if (found != std::string::npos) {
                vectorRes.push_back((double)(found + 1));
                start = found + 1;
            } else {
                break;
            }
        }
    }
    if (vectorRes.empty()) {
        res = ArrayOf::emptyConstructor();
    } else {
        size_t Clen = vectorRes.size();
        double* Cp = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, Clen, stringVector(), false));
        Dimensions dimC(1, Clen);
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
            Cp[k] = vectorRes[k];
        }
        res = ArrayOf(NLS_DOUBLE, dimC, Cp);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
