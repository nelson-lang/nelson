//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/container/vector.hpp>
#include "nlsConfig.h"
#include "StringFind.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
StringFind(const std::wstring& A, const std::wstring& B)
{
    ArrayOf res;
    boost::container::vector<double> vectorRes;
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
