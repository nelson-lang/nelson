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
#include <tuple>
#include <set>
#include "nlsProfiler_exports.h"
#include "Types.hpp"
#include "Evaluator.hpp"
#include "ProfilerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPROFILER_IMPEXP Profiler
{
public:
    enum Profile_Sort_Type
    {
        SORT_BY_NAME,
        SORT_BY_FILENAME,
        SORT_BY_NBCALLS,
        SORT_BY_TOTALTIME,
        SORT_BY_PERCALL,
        SORT_BY_LINE,
        SORT_BY_NAMEFILELINE
    };
    static Profiler*
    getInstance();

    void
    on();

    void
    off();

    bool
    isOn();

    void
    resume();

    void
    clear();

    uint64
    tic();

    void
    toc(uint64 tic, internalProfileFunction stack);

    std::vector<std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>>
    info(Profiler::Profile_Sort_Type sortOption);

    void
    show(Interface* io, Profiler::Profile_Sort_Type sortOption);

private:
    bool profileOn = false;

    std::unordered_map<size_t, profileFunction> profileMap;
    uint64 index = 0;

    Profiler();
    static Profiler* m_pInstance;

    uint64
    now();

    size_t
    hash(internalProfileFunction stack);
};
//=============================================================================
}
//=============================================================================
