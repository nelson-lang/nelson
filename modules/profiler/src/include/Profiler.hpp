//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#pragma once
//=============================================================================
#include <vector>
#include <unordered_map>
#include <string>
#include "nlsProfiler_exports.h"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPROFILER_IMPEXP Profiler
{
public:
    static Profiler*
    getInstance();

    void
    on();

    void
    off();

    void
    resume();

    void
    clear();

    void
    tic(const std::string &functionName, const std::wstring &filename);

    void
    toc(const std::string& functionName, const std::wstring& filename);

    std::unordered_map<std::string, std::vector<uint64>>
    info();

private:
    bool profileOn = false;
    Profiler();
    static Profiler* m_pInstance;
    std::unordered_map<std::string, std::vector<uint64>> profiling;
    uint64
    now();
};
//=============================================================================
}
//=============================================================================
