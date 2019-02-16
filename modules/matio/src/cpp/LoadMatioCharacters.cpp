//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <iostream>
#include <string>
#include <locale>
#include <codecvt>
#include "LoadMatioCharacters.hpp"
#include "characters_encoding.hpp"
#include "Exception.hpp"
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
    Dimensions dims;
    for (int d = 0; d < matVariable->rank; d++) {
        dims[d] = matVariable->dims[d];
    }
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(NLS_CHAR);
        bSuccess = true;
    } else {
        void* ptr = nullptr;
        Class classAsInteger;
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
    case NLS_UINT32:{
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
