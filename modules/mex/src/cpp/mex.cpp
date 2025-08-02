//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include <cstring>
#include "mex.h"
#include "NelsonPrint.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "Warning.hpp"
//=============================================================================
static std::string currentMexFunctionName;
//=============================================================================
int
mexPrintf(const char* format, ...)
{
    if (format == nullptr) {
        return 0;
    }
    std::vector<char> buff;
    size_t len = strlen(format);
    size_t size = 1024;
    if (len >= 1024) {
        size = len * 2;
    }
    buff.resize(size);
    va_list ap;
    va_start(ap, format);
    while (true) {
#ifdef _MSC_VER
        int ret = _vsnprintf_s(buff.data(), size, _TRUNCATE, format, ap);
#else
        int ret = vsnprintf(buff.data(), size, format, ap);
#endif
        if (ret != -1) {
            break;
        }
        size *= 2;
        buff.resize(size);
    }
    va_end(ap);
    std::wstring wstr = Nelson::utf8_to_wstring(buff.data());
    NelsonPrint(wstr.c_str());
    return (int)buff.size();
}
//=============================================================================
void
mexErrMsgTxt(const char* error_msg)
{
    Nelson::Error(error_msg);
}
//=============================================================================
void
mexErrMsgIdAndTxt(const char* identifier, const char* err_msg, ...)
{
    std::vector<char> buff;
    size_t len = strlen(err_msg);
    size_t size = 1024;
    if (len >= 1024) {
        size = len * 2;
    }
    buff.resize(size);
    va_list ap;
    va_start(ap, err_msg);
    while (true) {
#ifdef _MSC_VER
        int ret = _vsnprintf_s(buff.data(), size, _TRUNCATE, err_msg, ap);
#else
        int ret = vsnprintf(buff.data(), size, err_msg, ap);
#endif
        if (ret != -1) {
            break;
        }
        size *= 2;
        buff.resize(size);
    }
    va_end(ap);
    std::string msg = buff.data();
    Nelson::Error(msg, identifier);
}
//=============================================================================
void
mexWarnMsgTxt(const char* warn_msg)
{
    Nelson::Warning(warn_msg);
}
//=============================================================================
void
mexWarnMsgIdAndTxt(const char* warningid, const char* warningmsg, ...)
{
    std::vector<char> buff;
    size_t len = strlen(warningmsg);
    size_t size = 1024;
    if (len >= 1024) {
        size = len * 2;
    }
    buff.resize(size);
    va_list ap;
    va_start(ap, warningmsg);
    while (true) {
#ifdef _MSC_VER
        int ret = _vsnprintf_s(buff.data(), size, _TRUNCATE, warningmsg, ap);
#else
        int ret = vsnprintf(buff.data(), size, warningmsg, ap);
#endif
        if (ret != -1) {
            break;
        }
        size *= 2;
        buff.resize(size);
    }
    va_end(ap);
    Nelson::Warning(warningid, buff.data());
}
//=============================================================================
