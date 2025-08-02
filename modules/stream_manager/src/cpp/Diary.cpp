//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include <fstream>
#include <iostream>
#include "Diary.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
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
    FileSystemWrapper::Path p(wFilename);
    bool previouslyExist = p.is_regular_file();
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
        FileSystemWrapper::Path p(wFilename);
        FileSystemWrapper::Path::remove(p);
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
            StringHelpers::replace_all(utf8msg, "\n", "\r\n");
            StringHelpers::replace_all(utf8msg, "\r\r", "\r");
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
