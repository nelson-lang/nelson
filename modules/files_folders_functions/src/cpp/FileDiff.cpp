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
#include "FileDiff.hpp"
#include "characters_encoding.hpp"
#include "dtl/dtl.hpp"
#include <boost/algorithm/string.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
using dtl::Diff;
using dtl::elemInfo;
using dtl::uniHunk;
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
bool
FileDiff(std::wstring filename1, std::wstring filename2, logical eolcompare, std::wstring& res)
{
    typedef std::string elem;
    typedef std::vector<elem> sequence;
    typedef std::pair<elem, elemInfo> sesElem;
#ifdef _MSC_VER
    std::ifstream Aifs(filename1.c_str(), std::ios::in | std::ios::binary);
    std::ifstream Bifs(filename2.c_str(), std::ios::in | std::ios::binary);
#else
    std::ifstream Aifs(wstring_to_utf8(filename1).c_str(), std::ios::in | std::ios::binary);
    std::ifstream Bifs(wstring_to_utf8(filename2).c_str(), std::ios::in | std::ios::binary);
#endif
    elem buf;
    sequence ALines, BLines;
    if (!eolcompare) {
        while (safegetline(Aifs, buf)) {
            ALines.push_back(buf);
        }
        while (safegetline(Bifs, buf)) {
            BLines.push_back(buf);
        }
    } else {
        while (getline(Aifs, buf)) {
            ALines.push_back(buf);
        }
        while (getline(Bifs, buf)) {
            BLines.push_back(buf);
        }
    }
    Diff<elem> diff(ALines, BLines);
    diff.onHuge();
    diff.compose();
    // type unihunk definition test
    uniHunk<sesElem> hunk;
    diff.composeUnifiedHunks();
    std::ostringstream stream;
    diff.printUnifiedFormat(stream);
    res = utf8_to_wstring(stream.str());
    return true;
}
//=============================================================================
}
//=============================================================================
