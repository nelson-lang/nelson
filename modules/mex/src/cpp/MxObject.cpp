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
#include <cstring>
#include "mex.h"
#include "MxObject.h"
#include "MxTypes.h"
#include "ArrayOf.hpp"
#include "ClassName.hpp"
//=============================================================================
bool
mxIsClass(const mxArray* pm, const char* classname)
{
    bool res = false;
    if (pm == nullptr) {
        return false;
    }
    if (strcmp(classname, "cell") == 0) {
        return (pm->classID == mxCELL_CLASS);
    }
    if (strcmp(classname, "char") == 0) {
        return (pm->classID == mxCHAR_CLASS);
    }
    if (strcmp(classname, "double") == 0) {
        return (pm->classID == mxDOUBLE_CLASS);
    }
    if (strcmp(classname, "function_handle") == 0) {
        return (pm->classID == mxFUNCTION_CLASS);
    }
    if (strcmp(classname, "int8") == 0) {
        return (pm->classID == mxINT8_CLASS);
    }
    if (strcmp(classname, "int16") == 0) {
        return (pm->classID == mxINT16_CLASS);
    }
    if (strcmp(classname, "int32") == 0) {
        return (pm->classID == mxINT32_CLASS);
    }
    if (strcmp(classname, "int64") == 0) {
        return (pm->classID == mxINT64_CLASS);
    }
    if (strcmp(classname, "logical") == 0) {
        return (pm->classID == mxLOGICAL_CLASS);
    }
    if (strcmp(classname, "single") == 0) {
        return (pm->classID == mxSINGLE_CLASS);
    }
    if (strcmp(classname, "struct") == 0) {
        return (pm->classID == mxSTRUCT_CLASS);
    }
    if (strcmp(classname, "uint8") == 0) {
        return (pm->classID == mxUINT8_CLASS);
    }
    if (strcmp(classname, "uint16") == 0) {
        return (pm->classID == mxUINT16_CLASS);
    }
    if (strcmp(classname, "uint32") == 0) {
        return (pm->classID == mxUINT32_CLASS);
    }
    if (strcmp(classname, "uint64") == 0) {
        return (pm->classID == mxUINT64_CLASS);
    }
    if (strcmp(classname, "unknown") == 0) {
        return (pm->classID == mxUNKNOWN_CLASS);
    }
    return res;
}
//=============================================================================
mxClassID
mxGetClassID(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID;
    }
    return mxUNKNOWN_CLASS;
}
//=============================================================================
const char*
mxGetClassName(const mxArray* pm)
{
    if (pm != nullptr) {
        if (pm->ptr != nullptr) {
            auto* ptr = (Nelson::ArrayOf*)pm->ptr;
            if (ptr->isClassStruct()) {
                std::string name = Nelson::ClassName(*ptr);
                return name.c_str();
            }
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetClassName(mxArray* array_ptr, const char* classname)
{
    if (array_ptr != nullptr) {
        if (array_ptr->ptr != nullptr) {
            auto* ptr = (Nelson::ArrayOf*)array_ptr->ptr;
            if (ptr->isStruct()) {
                ptr->setStructType(classname);
                return 0;
            }
            return -3;
        }
        return -2;
    }
    return -1;
}
//=============================================================================
mxArray*
mxGetProperty(const mxArray* pa, mwIndex index, const char* propname)
{
    mxArray* res = nullptr;
    if (pa != nullptr) {
        if (pa->ptr != nullptr) {
        }
    }
    return res;
}
//=============================================================================
void
mxSetProperty(mxArray* pa, mwIndex index, const char* propname, const mxArray* value)
{
    if (pa != nullptr) {
        if (pa->ptr != nullptr) {
        }
    }
}
//=============================================================================
