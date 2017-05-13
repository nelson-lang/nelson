//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "characters_encoding.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    std::string ClassName(ArrayOf In)
    {
        std::string classString("");
        switch (In.getDataClass())
        {
            case NLS_HANDLE:
            {
                classString = NLS_HANDLE_STR;
                /* handle can be 'handle' or another type but not mixed */
                Dimensions dimsIn = In.getDimensions();
                nelson_handle *qp = (nelson_handle*)In.getDataPointer();
                if (qp)
                {
                    for (size_t k = 0; k < dimsIn.getElementCount(); k++)
                    {
                        nelson_handle hl = qp[k];
                        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
                        if (hlObj != nullptr)
                        {
                            std::string current = wstring_to_utf8(hlObj->getCategory());
                            if (classString != current && current != NLS_HANDLE_STR)
                            {
                                classString = current;
                            }
                        }
                    }
                }
            }
            break;
            case NLS_CELL_ARRAY:
            {
                classString = NLS_CELL_ARRAY_STR;
            }
            break;
            case NLS_STRUCT_ARRAY:
            {
                classString = In.getStructType();
            }
            break;
            case NLS_DCOMPLEX:
            case NLS_DOUBLE:
            {
                classString = NLS_DOUBLE_STR;
            }
            break;
            case NLS_SCOMPLEX:
            case NLS_SINGLE:
            {
                classString = NLS_SINGLE_STR;
            }
            break;
            case NLS_LOGICAL:
                classString = NLS_LOGICAL_STR;
                break;
            case NLS_UINT8:
                classString = NLS_UINT8_STR;
                break;
            case NLS_INT8:
                classString = NLS_INT8_STR;
                break;
            case NLS_UINT16:
                classString = NLS_UINT16_STR;
                break;
            case NLS_INT16:
                classString = NLS_INT16_STR;
                break;
            case NLS_UINT32:
                classString = NLS_UINT32_STR;
                break;
            case NLS_INT32:
                classString = NLS_INT32_STR;
                break;
            case NLS_UINT64:
                classString = NLS_UINT64_STR;
                break;
            case NLS_INT64:
                classString = NLS_INT64_STR;
                break;
            case NLS_CHAR:
                classString = NLS_CHAR_STR;
                break;
            default:
                break;
        }
        if ((In.is2D() || In.isEmpty()  || In.isScalar()) && !In.isSparse())
        {
            return classString;
        }
        else if (In.isSparse())
        {
            classString = std::string(NLS_SPARSE_STR) + classString;
        }
        else
        {
            classString = std::string(NLS_NDARRAY_STR) + classString;
        }
        return classString;
    }
    //=============================================================================
    stringVector ClassName(ArrayOfVector In)
    {
        stringVector strs;
        for (sizeType k = 0; k < (sizeType)In.size(); k++)
        {
            strs.push_back(ClassName(In[k]));
        }
        return strs;
    }
    //=============================================================================
    void ClassName(ArrayOf In, std::wstring &returnedClassName)
    {
        returnedClassName = utf8_to_wstring(ClassName(In));
    }
    //=============================================================================
    void ClassName(ArrayOf In, std::string &returnedClassName)
    {
        returnedClassName = ClassName(In);
    }
    //=============================================================================
}
//=============================================================================
