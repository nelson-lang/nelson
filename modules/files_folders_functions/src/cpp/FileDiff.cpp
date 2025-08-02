//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <dtl/dtl.hpp>
#include "StringHelpers.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include "FileDiff.hpp"
#include "characters_encoding.hpp"
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
        if (!myline.empty() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
bool
FileDiff(const std::wstring& filename1, const std::wstring& filename2, logical eolcompare,
    std::wstring& res)
{
    using elem = std::string;
    using sequence = std::vector<elem>;
    using sesElem = std::pair<elem, elemInfo>;
#ifdef _MSC_VER
    std::ifstream Aifs(filename1.c_str(), std::ios::in | std::ios::binary);
    std::ifstream Bifs(filename2.c_str(), std::ios::in | std::ios::binary);
#else
    std::ifstream Aifs(wstring_to_utf8(filename1).c_str(), std::ios::in | std::ios::binary);
    std::ifstream Bifs(wstring_to_utf8(filename2).c_str(), std::ios::in | std::ios::binary);
#endif
    elem buf;
    sequence ALines, BLines;
    if (eolcompare == 0U) {
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
} // namespace Nelson
//=============================================================================
