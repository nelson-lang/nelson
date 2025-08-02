//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <bitset>
#include "Error.hpp"
#include "i18n.hpp"
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
    bool supportedType = (A.isNumeric() || A.isLogical());
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
    NelsonType dataClass = A.getDataClass();
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
            indexType elementCount = dims.getElementCount();
            for (indexType k = 0; k < elementCount; ++k) {
                std::string cstr = std::bitset<1>(values[k]).to_string();
                vs.push_back(cstr);
            }
        } break;
        case NLS_DOUBLE: {
            auto* values = (double*)A.getDataPointer();
            indexType elementCount = dims.getElementCount();
            for (indexType k = 0; k < elementCount; ++k) {
                unsigned long long bits = *reinterpret_cast<unsigned long long*>(&values[k]);
                std::string cstr = std::bitset<64>(bits).to_string();
                vs.push_back(cstr);
            }
        } break;
        case NLS_SINGLE: {
            auto* values = (single*)A.getDataPointer();
            indexType elementCount = dims.getElementCount();
            for (indexType k = 0; k < elementCount; ++k) {
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
