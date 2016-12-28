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
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "dtl/dtl.hpp"
#include "FileDiff.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    using dtl::Diff;
    using dtl::elemInfo;
    using dtl::uniHunk;
    //=============================================================================

    bool FileDiff(std::wstring filename1, std::wstring filename2, logical eolcompare, std::wstring &res)
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
        while (getline(Aifs, buf))
        {
            if (!eolcompare)
            {
                boost::replace_all(buf,  "\r\n", "\n");
            }
            ALines.push_back(buf);
        }
        while (getline(Bifs, buf))
        {
            if (!eolcompare)
            {
                boost::replace_all(buf, "\r\n", "\n");
            }
            BLines.push_back(buf);
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
