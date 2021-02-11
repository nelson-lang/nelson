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
#include "ToLower.hpp"
#include "Error.hpp"
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <cctype>
#include <string>
#include "nlsConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToLower(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isRowVectorCharacterArray()) {
        return ArrayOf::characterArrayConstructor(ToLower(A.getContentAsWideString()));
    }
    if (A.getDataClass() == NLS_CELL_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        }
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.getDataPointer());
        indexType elementCount = A.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!element[k].isRowVectorCharacterArray()) {
                Error(ERROR_TYPE_CELL_OF_STRINGS_EXPECTED);
            }
            element[k]
                = ArrayOf::characterArrayConstructor(ToLower(element[k].getContentAsWideString()));
        }
        return res;
    }
    if (A.getDataClass() == NLS_STRING_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        }
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.getDataPointer());
        ompIndexType elementCount = A.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (element[k].isRowVectorCharacterArray()) {
                element[k] = ArrayOf::characterArrayConstructor(
                    ToLower(element[k].getContentAsWideString()));
            } else {
                element[k] = ArrayOf::emptyConstructor();
            }
        }
        return res;
    }
    needToOverload = true;

    return res;
}
//=============================================================================
std::wstring
ToLower(const std::wstring& A)
{
    return boost::to_lower_copy(A);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
