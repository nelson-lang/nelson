//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "LoadMatioCharacters.hpp"
#include "characters_encoding.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
integerToCharType(ArrayOf& AasInteger, bool asUtf, ArrayOf& AasCharType);
//=============================================================================
bool
LoadMatioCharacters(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(NLS_CHAR);
        bSuccess = true;
    } else {
        void* ptr = nullptr;
        NelsonType classAsInteger;
        bool asUtf = false;
        switch (matVariable->data_type) {
        case MAT_T_UTF8: {
            asUtf = true;
            classAsInteger = NLS_UINT8;
        } break;
        case MAT_T_UINT8: {
            classAsInteger = NLS_UINT8;
        } break;
        case MAT_T_UTF16: {
            asUtf = true;
            classAsInteger = NLS_UINT16;
        } break;
        case MAT_T_UINT16: {
            classAsInteger = NLS_UINT16;
        } break;
        case MAT_T_UTF32: {
            asUtf = true;
            classAsInteger = NLS_UINT32;
        } break;
        case MAT_T_UINT32: {
            classAsInteger = NLS_UINT32;
        } break;
        default: {
            return false;
        } break;
        }
        try {
            ptr = ArrayOf::allocateArrayOf(
                classAsInteger, dims.getElementCount(), stringVector(), false);
        } catch (Exception&) {
            return false;
        }
        memcpy(ptr, matVariable->data, matVariable->nbytes);
        ArrayOf VariableAsInteger = ArrayOf(classAsInteger, dims, ptr);
        bSuccess = integerToCharType(VariableAsInteger, asUtf, VariableValue);
    }
    return bSuccess;
}
//=============================================================================
bool
integerToCharType(ArrayOf& AasInteger, bool asUtf, ArrayOf& AasCharType)
{
    switch (AasInteger.getDataClass()) {
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32: {
        AasCharType = AasInteger;
        AasCharType.promoteType(NLS_CHAR);
        return true;
    } break;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
