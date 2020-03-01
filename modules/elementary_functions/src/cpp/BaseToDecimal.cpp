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
#include <algorithm>
#include <cwchar>
#include <boost/algorithm/string/erase.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/unordered_map.hpp>
#include "Error.hpp"
#include "Exception.hpp"
#include "BaseToDecimal.hpp"
#include "characters_encoding.hpp"
#include "Transpose.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::unordered_map<std::wstring, double> baseConverted;
static size_t lastBase = 10;
//=============================================================================
uint64
base2dec(const std::wstring wstr, int base)
{
    uint64 res = 0;
    if (wstr.empty()) {
        return res;
    }
    wchar_t* str = nullptr;
    res = wcstoull(wstr.c_str(), &str, base);
    if (wcslen(str) > 0) {
        Error(_W("Cannot convert: ") + wstr);
    }
    return res;
}
//=============================================================================
static double
BaseToDecimal(const std::wstring& element, int base)
{
    double res;
    if (element.empty()) {
        res = 0;
    } else {
        std::wstring upperCase = boost::algorithm::erase_all_copy(element, L" ");
        std::transform(upperCase.begin(), upperCase.end(), upperCase.begin(), towupper);
        res = (double)base2dec(upperCase, base);
    }
    return res;
}
//=============================================================================
ArrayOf
BaseToDecimal(ArrayOf& A, ArrayOf& Base, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    double dbase = Base.getContentAsDoubleScalar();
    double fbase = std::floor(dbase);
    if (dbase != fbase) {
        Error(_W("The base must be an integer value between 2 and 36."));
    }
    int ibase = static_cast<int>(fbase);
    if (ibase < 2 || ibase > 36) {
        Error(_W("The base must be an integer value between 2 and 36."));
    }
    if (lastBase != ibase) {
        baseConverted.clear();
        lastBase = ibase;
    }
    Dimensions dimsA = A.getDimensions();
    if (dimsA.isEmpty(false)) {
        baseConverted.clear();
        res = ArrayOf::emptyConstructor();
    } else {
        wstringVector elements;
        switch (A.getDataClass()) {
        case NLS_CHAR: {
            if (A.isRowVector()) {
                elements.push_back(A.getContentAsWideString());
            } else {
                if (A.isNdArrayCharacterType()) {
                    Error(_W("2D char array expected."));
                }
                bool dummy;
                ArrayOf Transposed = Transpose(A, dummy);
                std::wstring wstr = Transposed.getContentAsArrayOfCharacters();
                size_t commonLength = dimsA.getColumns();
                elements.reserve(dimsA.getElementCount() / commonLength);
                indexType elementCount = dimsA.getElementCount();
                for (indexType k = 0; k < elementCount; k = k + commonLength) {
                    std::wstring s = wstr.substr(k, commonLength);
                    elements.push_back(s);
                }
            }
        } break;
        case NLS_CELL_ARRAY: {
            elements = A.getContentAsWideStringVector(false);
        } break;
        case NLS_STRING_ARRAY: {
            elements = A.getContentAsWideStringVector(false);
        } break;
        default: {
            needToOverload = true;
            return ArrayOf();
        } break;
        }
        Dimensions dims(elements.size(), 1);
        double* ptr
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elements.size(), stringVector(), false);
        res = ArrayOf(NLS_DOUBLE, dims, ptr);
        size_t k = 0;
        for (std::wstring e : elements) {
            if (baseConverted.count(e) != 0) {
                ptr[k] = baseConverted[e];
            } else {
                if (baseConverted.size() > baseConverted.max_size() - 1000) {
                    baseConverted.clear();
                }
                ptr[k] = BaseToDecimal(e, ibase);
                baseConverted[e] = ptr[k];
            }
            k++;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
