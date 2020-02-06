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
#include <bitset>
#include <Eigen/Dense>
#include "Error.hpp"
#include "Exception.hpp"
#include "NumToBin.hpp"
#include "Transpose.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
NumToBin(ArrayOf A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    bool supportedType = A.isNumeric() || A.isLogical();
    if (!supportedType) {
        needToOverload = true;
        return res;
    }
    if (A.isComplex()) {
        Error(_W("First argument must be real."));
    }
    if (A.isSparse()) {
        A.makeDense();
    }
    Class dataClass = A.getDataClass();
    bool needToConvert
        = (dataClass != NLS_LOGICAL) && (dataClass != NLS_SINGLE) && (dataClass != NLS_DOUBLE);
    if (needToConvert) {
        A.promoteType(NLS_DOUBLE);
    }
    if (A.isEmpty(false)) {
        res = ArrayOf::characterArrayConstructor("");
    } else {
        Dimensions dims = A.getDimensions();
        stringVector vs;
        vs.reserve(dims.getElementCount());
        size_t len = 0;
        switch (A.getDataClass()) {
        case NLS_LOGICAL: {
            auto* values = (logical*)A.getDataPointer();
            for (indexType k = 0; k < dims.getElementCount(); ++k) {
                std::string cstr = std::bitset<1>(values[k]).to_string();
                vs.push_back(cstr);
            }
        } break;
        case NLS_DOUBLE: {
            auto* values = (double*)A.getDataPointer();
            for (indexType k = 0; k < dims.getElementCount(); ++k) {
                unsigned long long bits = *reinterpret_cast<unsigned long long*>(&values[k]);
                std::string cstr = std::bitset<64>(bits).to_string();
                vs.push_back(cstr);
            }
        } break;
        case NLS_SINGLE: {
            auto* values = (single*)A.getDataPointer();
            for (indexType k = 0; k < dims.getElementCount(); ++k) {
                unsigned long bits = *reinterpret_cast<unsigned long*>(&values[k]);
                std::string cstr = std::bitset<32>(bits).to_string();
                vs.push_back(cstr);
            }
        } break;
        default: {
            needToConvert = true;
            return res;
        } break;
        }
        res = ArrayOf::characterVectorToCharacterArray(vs);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
