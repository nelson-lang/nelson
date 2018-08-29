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
#include "Diary.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <fstream>
#include <iostream>
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
Diary::SetFilename(std::wstring wFilename)
{
    bool bRes = false;
    boost::filesystem::path p = wFilename;
    bool previouslyExist = boost::filesystem::exists(p) && !boost::filesystem::is_directory(p);
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
        boost::filesystem::path p = wFilename;
        try {
            boost::filesystem::remove(p);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                // ONLY FOR DEBUG
            }
        }
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
Diary::writeMessage(std::string msg)
{
    writeMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
Diary::writeMessage(std::wstring msg)
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
}
