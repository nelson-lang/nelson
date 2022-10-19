//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <fstream>
#include <iostream>
#include "Diary.hpp"
#include "characters_encoding.hpp"
#include "FileSystemHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Diary::Diary()
{
    diaryFilename = L"diary";
    bState = false;
}
//=============================================================================
Diary::~Diary()
{
    diaryFilename = L"diary";
    bState = false;
}
//=============================================================================
bool
Diary::SetFilename(const std::wstring& wFilename)
{
    bool bRes = false;
    Nelson::FileSystemWrapper::Path p(wFilename);
    bool previouslyExist = isFile(p);
    std::ios::openmode wofstream_mode = std::ios::app | std::ios::binary;
#ifdef _MSC_VER
    std::wofstream fileDiary(wFilename, wofstream_mode);
#else
    std::string filename = wstring_to_utf8(wFilename);
    std::ofstream fileDiary(filename, wofstream_mode);
#endif
    if (fileDiary.good()) {
        fileDiary.close();
        diaryFilename = wFilename;
        bRes = true;
    }
    if (!bState || !previouslyExist) {
        // remove create diary if state is off
        // or if diary did not exist before
        Nelson::FileSystemWrapper::Path p(wFilename);
        Nelson::FileSystemWrapper::Path::remove(p);
    }
    return bRes;
}
//=============================================================================
std::wstring
Diary::getFilename()
{
    return diaryFilename;
}
//=============================================================================
void
Diary::writeMessage(const std::string& msg)
{
    writeMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
Diary::writeMessage(const std::wstring& msg)
{
    if (bState) {
        std::ios::openmode wofstream_mode = std::ios::app | std::ios::binary;
#ifdef _MSC_VER
        std::wofstream fileDiary(diaryFilename, wofstream_mode);
#else
        std::string filename = wstring_to_utf8(diaryFilename);
        std::ofstream fileDiary(filename, wofstream_mode);
#endif
        if (fileDiary.good()) {
            std::string utf8msg = wstring_to_utf8(msg);
#ifdef _MSC_VER
            boost::replace_all(utf8msg, "\n", "\r\n");
            boost::replace_all(utf8msg, "\r\r", "\r");
#endif
            fileDiary << utf8msg.c_str();
            fileDiary.close();
        }
    }
}
//=============================================================================
bool
Diary::getState()
{
    return bState;
}
//=============================================================================
bool
Diary::setState(bool bNewState)
{
    bool bPrevious = bState;
    bState = bNewState;
    return bPrevious;
}
//=============================================================================
void
Diary::toggle()
{
    bState = !bState;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
