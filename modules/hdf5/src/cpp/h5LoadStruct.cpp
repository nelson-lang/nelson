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
#include "h5LoadStruct.hpp"
#include "h5LoadFunctionHandle.hpp"
#include "h5LoadString.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5LoadVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadStruct(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    const Dimensions &dims, bool isObject, const std::string& objectClassname, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (objectClassname == NLS_FUNCTION_HANDLE_STR && isObject) {
        bSuccess = h5LoadFunctionHandle(fid, location, variableName, isEmpty, dims, VariableValue);
    } else {
        std::string h5path;
        if (location == "/") {
            h5path = location + variableName;
        } else {
            h5path = location + "/" + variableName;
        }
        Dimensions dimsFieldnames = getNelsonDimensions(fid, h5path, "fieldnames");
        bool isEmptyFieldnames = isNelsonEmpty(fid, h5path, "fieldnames");
        stringVector fieldnames;
        ArrayOf fieldnamesArrayOf;
        if (h5LoadStringArray(
                fid, h5path, "fieldnames", isEmptyFieldnames, dimsFieldnames, fieldnamesArrayOf)) {
            fieldnames = fieldnamesArrayOf.getContentAsCStringVector(false);
        } else {
            return false;
        }
        indexType nbElements = dims.getElementCount();
        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements * fieldnames.size()];
        } catch (const std::bad_alloc&) {
            return false;
        }
        VariableValue = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        indexType length = dims.getElementCount();
        indexType offset = 0;
        for (indexType j = 0; j < length; j++) {
            for (indexType i = 0; i < (sizeType)fieldnames.size(); i++) {
                std::string name = std::to_string(offset);
                bool bSuccess
                    = h5LoadVariable(fid, h5path + std::string("/"), name, elements[offset]);
                if (!bSuccess) {
                    return false;
                }
                offset++;
            }
        }
        bSuccess = true;
        if (isObject) {
            VariableValue.setStructType(objectClassname);
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
