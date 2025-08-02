//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <algorithm>
#include <cstring>
#include "mex.h"
#include "matrix.h"
#include "MxHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
mxArray*
mxCreateStringFromNChars(const char* str, mwSize n)
{
    size_t lenStr = strlen(str);
    size_t len = n;
    if (n > lenStr) {
        len = lenStr;
    } else if (n == lenStr) {
        len = lenStr;
    }
    std::string s = str;
    std::string sub = s.substr(0, len);
    return mxCreateString(sub.c_str());
}
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
    mwSize i;
    mwSize j;
    mxChar* ptr;

    for (i = 0; i < m; i++) {
        maxlen = std::max(maxlen, strlen(str[i]));
    }
    dims[0] = m;
    dims[1] = maxlen;
    mxArray* res = mxCreateCharArray(2, dims);
    ptr = (mxChar*)res->realdata;
    for (i = 0; i < m; i++) {
        auto lenStr = (mwSize)strlen(str[i]);
        for (j = 0; j < lenStr; j++) {
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
void
mxGetNChars(const mxArray* pa, char* buf, mwSize nChars)
{
    if (pa->classID == mxCHAR_CLASS) {
        auto* p = (mxChar*)(pa->realdata);
        size_t N = mxGetNumberOfElements(pa);
        size_t L = nChars;
        if (nChars > N) {
            L = N;
        }
        if (buf != nullptr) {
            for (size_t i = 0; i < L; i++) {
                buf[i] = (char)p[i];
            }
            buf[L] = 0;
        }
    }
}
//=============================================================================
char*
mxArrayToString(const mxArray* array_ptr)
{
    if (array_ptr->classID != mxCHAR_CLASS) {
        return nullptr;
    }
    auto* p = (mxChar*)(array_ptr->realdata);
    size_t N = mxGetNumberOfElements(array_ptr);
    char* res = (char*)mxCalloc(N + 1, sizeof(char));
    if (res != nullptr) {
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
    auto* p = (mxChar*)(array_ptr->realdata);
    size_t N = mxGetNumberOfElements(array_ptr);
    wchar_t* res = nullptr;
    try {
        res = new wchar_t[N + (size_t)1];
    } catch (const std::bad_alloc&) {
        res = nullptr;
    }
    if (res != nullptr) {
        for (size_t i = 0; i < N; i++) {
            res[i] = p[i];
        }
        res[N] = 0;
        std::string ustr = Nelson::wstring_to_utf8(res);
        delete[] res;
        char* utfres = (char*)mxCalloc((ustr.size() + 1), sizeof(char));
        if (utfres != nullptr) {
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
    size_t elcount;
    size_t tocopy;
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
    if (pm != nullptr) {
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
