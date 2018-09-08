//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "ClassName.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "ClassToString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
ClassName(ArrayOf In)
{
    std::string classString = wstring_to_utf8(ClassToString(In.getDataClass()));
    if (In.getDataClass() == NLS_HANDLE) {
        classString = NLS_HANDLE_STR;
        /* handle can be 'handle' or another type but not mixed */
        Dimensions dimsIn = In.getDimensions();
        nelson_handle* qp = (nelson_handle*)In.getDataPointer();
        if (qp) {
            for (size_t k = 0; k < dimsIn.getElementCount(); k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj != nullptr) {
                    std::string current = wstring_to_utf8(hlObj->getCategory());
                    if (classString != current && current != NLS_HANDLE_STR) {
                        classString = current;
                    }
                }
            }
        }
    } else if (In.getDataClass() == NLS_STRUCT_ARRAY) {
        classString = In.getStructType();
    }
    if (In.isSparse()) {
        classString = std::string(NLS_SPARSE_STR) + classString;
    }
    return classString;
}
//=============================================================================
stringVector
ClassName(ArrayOfVector In)
{
    stringVector strs;
    for (sizeType k = 0; k < (sizeType)In.size(); k++) {
        strs.push_back(ClassName(In[k]));
    }
    return strs;
}
//=============================================================================
void
ClassName(ArrayOf In, std::wstring& returnedClassName)
{
    returnedClassName = utf8_to_wstring(ClassName(In));
}
//=============================================================================
void
ClassName(ArrayOf In, std::string& returnedClassName)
{
    returnedClassName = ClassName(In);
}
//=============================================================================
}
//=============================================================================
