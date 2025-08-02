//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    toc(uint64 tic, const internalProfileFunction& stack);

    std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>>
    info(Profiler::Profile_Sort_Type sortOption);

    void
    show(Interface* io, Profiler::Profile_Sort_Type sortOption, int nbLinesToDisplay = -1);

    void
    save(std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>>
             profileInfo,
        const std::wstring& destinationDirectory, const std::wstring& moduleProfilerPath,
        std::wstring& errorMessage);

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

    std::vector<std::tuple<int, double>>
    getInfoForContent(
        const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
        const std::wstring& filename, size_t contentSize);

    bool
    getInfoForLine(const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
        const std::wstring& filename, size_t line, int& numcalls, double& time);

    stringVector
    readFunction(const std::wstring& filename);

    std::tuple<int, double>
    computeBasicFileStats(
        const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
        const stringVector& functionContent, const std::wstring& srcFilename);

    std::vector<std::tuple<int, std::string, int, double>>
    getFiveLinesConsumingMostTime(
        const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
        const std::wstring& srcFilename, const stringVector& functionContent);

    std::vector<std::tuple<uint64, uint64, uint64>>
    getProfileForFile(
        const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
        const std::wstring& srcFilename);

    std::tuple<int, int, int, int, int, double>
    coverageAnalyzer(
        const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
        const std::wstring& srcFilename, const stringVector& functionContent);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
