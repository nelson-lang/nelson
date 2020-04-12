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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <algorithm>
#include <string.h>
#include "mex.h"
#include "matrix.h"
#include "MxHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
mxArray*
mxCreateString(const char* str)
{
    return mxCreateCharMatrixFromStrings(1, &str);
}
//=============================================================================
mxArray*
mxCreateCharMatrixFromStrings(mwSize m, const char** str)
{
    mwSize dims[2];
    size_t maxlen = 0;
    mwSize i, j;
    mxChar* ptr;

    for (i = 0; i < m; i++) {
        maxlen = std::max(maxlen, strlen(str[i]));
    }
    dims[0] = m;
    dims[1] = maxlen;
    mxArray* res = mxCreateCharArray(2, dims);
    ptr = (mxChar*)res->realdata;
    for (i = 0; i < m; i++) {
        for (j = 0; j < strlen(str[i]); j++) {
            ptr[i + j * m] = str[i][j];
        }
    }
    return res;
}
//=============================================================================
mxArray*
mxCreateCharArray(mwSize ndim, const mwSize* dims)
{
    return mxAllocateRealArray(ndim, dims, sizeof(mxChar), mxCHAR_CLASS);
}
//=============================================================================

char*
mxArrayToString(const mxArray* array_ptr)
{
    if (array_ptr->classID != mxCHAR_CLASS) {
        return nullptr;
    }
    mxChar* p = (mxChar*)(array_ptr->realdata);
    size_t N = mxGetNumberOfElements(array_ptr);
    char* res = (char*)malloc(N + 1);
    if (res) {
        for (size_t i = 0; i < N; i++) {
            res[i] = (char)p[i];
        }
        res[N] = 0;
    }
    return res;
}
//=============================================================================
char*
mxArrayToUTF8String(const mxArray* array_ptr)
{
    if (array_ptr->classID != mxCHAR_CLASS) {
        return nullptr;
    }
    mxChar* p = (mxChar*)(array_ptr->realdata);
    size_t N = mxGetNumberOfElements(array_ptr);
    wchar_t* res = new wchar_t[N + (size_t)1];
    if (res) {
        for (size_t i = 0; i < N; i++) {
            res[i] = p[i];
        }
        res[N] = 0;
        std::string ustr = Nelson::wstring_to_utf8(res);
        delete[] res;
        char* utfres = (char*)malloc(sizeof(char) * (ustr.size() + 1));
        if (utfres) {
            strcpy(utfres, ustr.c_str());
        }
        return utfres;
    }
    return nullptr;
}
//=============================================================================
int
mxGetString(const mxArray* pm, char* str, mwSize strlen)
{
    mxChar* ptr;
    size_t i;
    size_t elcount, tocopy;
    if (pm->classID != mxCHAR_CLASS) {
        return 1;
    }
    ptr = (mxChar*)pm->realdata;
    elcount = mxGetNumberOfElements(pm);
    tocopy = std::min((mwSize)elcount, (mwSize)(strlen - 1));
    for (i = 0; i < tocopy; i++) {
        str[i] = (char)ptr[i];
    }
    str[tocopy] = 0;
    if (tocopy < elcount) {
        return 1;
    }
    return 0;
}
//=============================================================================
bool
mxIsChar(const mxArray* pm)
{
    if (pm) {
        return (pm->classID == mxCHAR_CLASS);
    }
    return false;
}
//=============================================================================
mxChar*
mxGetChars(const mxArray* array_ptr)
{
    return (mxChar*)array_ptr->realdata;
}
//=============================================================================