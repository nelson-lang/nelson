//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <chrono>
#include <iomanip>
#include <tuple>
#include <iostream>
#include <fstream>
#include "StringHelpers.hpp"
#include "Profiler.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "HtmlExporter.hpp"
#include "FileSystemWrapper.hpp"
#include "i18n.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Profiler* Profiler::m_pInstance = nullptr;
//=============================================================================
Profiler::Profiler() { clear(); }
//=============================================================================
Profiler*
Profiler::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new Profiler();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
Profiler::on()
{
    clear();
    resume();
}
//=============================================================================
void
Profiler::off()
{
    profileOn = false;
}
//=============================================================================
void
Profiler::resume()
{
    profileOn = true;
}
//=============================================================================
bool
Profiler::isOn()
{
    return profileOn;
}
//=============================================================================
void
Profiler::clear()
{
    profileMap.clear();
}
//=============================================================================
uint64
Profiler::tic()
{
    if (!profileOn) {
        return 0;
    }
    return now();
}
//=============================================================================
void
Profiler::toc(uint64 tic, const internalProfileFunction& stack)
{
    if (!profileOn) {
        return;
    }
    if (std::get<1>(stack) == "profile") {
        return;
    }

    uint64 t = now();
    uint64 diff_time = t - tic;

    size_t id = hash(stack);
    auto search = profileMap.find(id);
    if (search != profileMap.end()) {
        std::get<1>(search->second) = std::get<1>(search->second) + 1;
        std::get<2>(search->second) = std::get<2>(search->second) + diff_time;
    } else {
        profileMap.insert(std::make_pair(id, std::make_tuple(stack, 1, diff_time, index)));
        index++;
    }
}
//=============================================================================
static bool
sortByPerCall(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 percallA = std::get<5>(a);
    uint64 percallB = std::get<5>(b);

    return percallA < percallB;
}
//=============================================================================
static bool
sortByNbCalls(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 nbcallsA = std::get<3>(a);
    uint64 nbcallsB = std::get<3>(b);
    return nbcallsA < nbcallsB;
}
//=============================================================================
static bool
sortByFilename(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string filenameA = std::get<0>(a);
    std::string filenameB = std::get<0>(b);

    return filenameA.compare(filenameB) < 0;
}
//=============================================================================
static bool
sortByLine(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 lineposA = std::get<1>(a);
    uint64 lineposB = std::get<1>(b);

    return lineposA < lineposB;
}
//=============================================================================
static bool
sortByName(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string nameA = std::get<2>(a);
    std::string nameB = std::get<2>(b);
    return nameA.compare(nameB) < 0;
}
//=============================================================================
static bool
sortByTotalTime(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 tottimeA = std::get<4>(a);
    uint64 tottimeB = std::get<4>(b);
    return tottimeA < tottimeB;
}
//=============================================================================
static bool
sortByNameFilenameLine(
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string nameA = std::get<2>(a);
    std::string nameB = std::get<2>(b);
    uint64 lineposA = std::get<1>(a);
    uint64 lineposB = std::get<1>(b);
    std::string filenameA = std::get<0>(a);
    std::string filenameB = std::get<0>(b);

    std::string lineAasStr = std::to_string(lineposA);
    std::string lineBasStr = std::to_string(lineposB);
    size_t m = std::max(lineAasStr.length(), lineBasStr.length());

    std::string A = filenameA + ":" + std::string(m - lineAasStr.length(), '0')
        + std::to_string(lineposA) + "(" + nameA + ")";
    std::string B = filenameB + ":" + std::string(m - lineBasStr.length(), '0')
        + std::to_string(lineposB) + "(" + nameB + ")";
    return A.compare(B) < 0;
}
//=============================================================================
std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>>
Profiler::info(Profiler::Profile_Sort_Type sortOption)
{
    std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>> profilerLines;

    for (const std::pair<size_t, profileFunction>& element : profileMap) {
        profileFunction pF = element.second;
        uint64 index = std::get<3>(pF);
        internalProfileFunction iPf = std::get<0>(pF);
        profileParentStack parentStack = std::get<0>(iPf);
        std::string name = std::get<1>(iPf);
        if (!parentStack.empty()) {
            profileParent lastParent = parentStack[parentStack.size() - 1];
            int lastLinePosition = (int)std::get<1>(lastParent);
            std::string lastFunctionName = std::get<0>(lastParent);
            int k = (int)parentStack.size();
            while (lastLinePosition == 0 && k - 1 >= 0) {
                k = k - 1;
                lastParent = parentStack[k];
                lastLinePosition = (int)std::get<1>(lastParent);
                lastFunctionName = std::get<0>(lastParent);
            }
            uint64 nbcalls = std::get<1>(pF);
            uint64 tottime = std::get<2>(pF);
            uint64 line = lastLinePosition;
            std::string filename = lastFunctionName;
            uint64 percall = tottime / nbcalls;

            std::tuple<std::string, uint64, std::string, uint64, uint64, uint64> value
                = std::make_tuple(filename, line, name, nbcalls, tottime, percall);

            bool found = false;
            for (auto& profilerLine : profilerLines) {
                if ((std::get<0>(value) == std::get<0>(profilerLine))
                    && (std::get<1>(value) == std::get<1>(profilerLine))
                    && (std::get<2>(value) == std::get<2>(profilerLine))) {
                    found = true;
                    std::get<3>(profilerLine) = std::get<3>(profilerLine) + std::get<3>(value);
                    std::get<4>(profilerLine) = std::get<4>(profilerLine) + std::get<4>(value);
                    std::get<5>(profilerLine)
                        = std::get<4>(profilerLine) / std::get<3>(profilerLine);
                }
            }
            if (!found) {
                profilerLines.push_back(value);
            }
        }
    }

    switch (sortOption) {
    case SORT_BY_LINE: {
        parallelSort(profilerLines, sortByLine);
    } break;
    case SORT_BY_NAMEFILELINE: {
        parallelSort(profilerLines, sortByNameFilenameLine);
    } break;
    case SORT_BY_NAME: {
        parallelSort(profilerLines, sortByName);
    } break;
    case SORT_BY_FILENAME: {
        parallelSort(profilerLines, sortByFilename);
    } break;
    case SORT_BY_NBCALLS: {
        parallelSort(profilerLines, sortByNbCalls);
    } break;
    case SORT_BY_PERCALL: {
        parallelSort(profilerLines, sortByPerCall);
    } break;
    case SORT_BY_TOTALTIME:
    default: {
        parallelSort(profilerLines, sortByTotalTime);
    } break;
    }
    return profilerLines;
}
//=============================================================================
uint64
Profiler::now()
{
    std::chrono::nanoseconds ns = std::chrono::high_resolution_clock::now().time_since_epoch();
    return uint64(static_cast<std::uint64_t>(ns.count()));
}
//=============================================================================
size_t
Profiler::hash(internalProfileFunction stack)
{
    std::string res;
    profileParentStack parent = std::get<0>(stack);
    for (auto& k : parent) {
        std::string filename = std::get<0>(k);
        int linePosition = (int)std::get<1>(k);
        res = res + filename + "|" + std::to_string(linePosition) + ">";
    }
    if (res.size()) {
        res.pop_back();
    }

    res = res + "[" + wstring_to_utf8(std::get<2>(stack)) + "|" + std::get<1>(stack) + "]";
    return std::hash<std::string> {}(res);
}
//=============================================================================
void
Profiler::show(Interface* io, Profiler::Profile_Sort_Type sortOption, int nbLinesToDisplay)
{
    // index, filename, line, name, nbcalls, tottime, percall
    std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>> profilerLines
        = Profiler::info(sortOption);

    std::stringstream ssLine;
    ssLine << std::endl;
    ssLine << std::right << std::setfill(' ') << std::setw(10) << _("NumCalls");
    ssLine << std::right << std::setfill(' ') << std::setw(16) << _("TotalTime (s)");
    ssLine << std::right << std::setfill(' ') << std::setw(16) << _("PerCall (s)");
    ssLine << "    " << std::left << std::setfill(' ') << std::setw(24)
           << _("Filename:LinePosition(FunctionName)");
    ssLine << std::endl;
    ssLine << std::endl;
    io->outputMessage(ssLine.str());

    int nbLinesDisplayed = 0;
    for (std::tuple<std::string, uint64, std::string, uint64, uint64, uint64> line :
        profilerLines) {
        if (nbLinesDisplayed < nbLinesToDisplay || nbLinesToDisplay == -1) {
            // filename, line, name, nbcalls, tottime, percall
            FileSystemWrapper::Path p(std::get<0>(line));
            std::string filename = p.filename().string();
            uint64 linepos = std::get<1>(line);
            std::string name = std::get<2>(line);
            uint64 nbcalls = std::get<3>(line);
            uint64 tottime = std::get<4>(line);
            uint64 percall = std::get<5>(line);
            std::stringstream ssLine;
            std::string flf = filename + ":" + std::to_string((int)linepos) + "(" + name + ")";

            ssLine << std::right << std::setfill(' ') << std::setw(10)
                   << std::to_string((int)nbcalls);
            ssLine << std::right << std::setfill(' ') << std::setw(16)
                   << std::to_string(tottime * 1e-9);
            ssLine << std::right << std::setfill(' ') << std::setw(16)
                   << std::to_string(percall * 1e-9);
            ssLine << "    " << std::left << std::setfill(' ') << std::setw(24) << flf;
            ssLine << std::endl;
            io->outputMessage(ssLine.str());
            nbLinesDisplayed++;
        }
    }
}
//=============================================================================
std::vector<std::tuple<int, double>>
Profiler::getInfoForContent(
    const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const std::wstring& filename, size_t contentSize)
{
    std::vector<std::tuple<int, double>> lines;
    for (size_t i = 0; i < contentSize; i++) {
        int numcalls;
        double time;
        if (!getInfoForLine(flatProfile, filename, i + 1, numcalls, time)) {
            numcalls = -1;
            time = -1;
        }
        lines.emplace_back(numcalls, time);
    }
    return lines;
}
//=============================================================================
bool
Profiler::getInfoForLine(
    const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const std::wstring& filename, size_t line, int& numcalls, double& time)
{
    for (const auto& k : flatProfile) {
        if (utf8_to_wstring(std::get<0>(k)) == filename && std::get<1>(k) == line) {
            numcalls = (int)std::get<2>(k);
            time = std::get<3>(k) * 1e-9;
            return true;
        }
    }
    return false;
}
//=============================================================================
stringVector
Profiler::readFunction(const std::wstring& filename)
{
    stringVector functionContent;
#ifdef _MSC_VER
    std::ifstream in(filename);
#else
    std::ifstream in(wstring_to_utf8(filename));
#endif
    std::string str;
    while (std::getline(in, str)) {
        functionContent.push_back(str);
    }
    return functionContent;
}
//=============================================================================
void
Profiler::save(
    std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>> profileInfo,
    const std::wstring& destinationDirectory, const std::wstring& moduleProfilerPath,
    std::wstring& errorMessage)
{
    std::wstring profileDirectory = destinationDirectory;
    if (!FileSystemWrapper::Path::is_directory(profileDirectory)
        && !FileSystemWrapper::Path::is_regular_file(profileDirectory)) {
        std::string message;
        if (!FileSystemWrapper::Path::create_directory(profileDirectory, message)) {
            errorMessage = utf8_to_wstring(message);
            return;
        }
    }
    // filename, line, time, calls
    std::vector<std::tuple<std::string, uint64, uint64, uint64>> flatProfile;
    // filename, line, name, nbcalls, tottime, percall
    for (std::tuple<std::string, uint64, std::string, uint64, uint64, uint64> element :
        profileInfo) {
        std::tuple<std::string, uint64, uint64, uint64> value = std::make_tuple(
            std::get<0>(element), std::get<1>(element), std::get<3>(element), std::get<4>(element));
        bool found = false;
        for (auto& k : flatProfile) {
            if ((std::get<0>(value) == std::get<0>(k)) && (std::get<1>(value) == std::get<1>(k))) {
                found = true;
                std::get<3>(k) = std::get<3>(k) + std::get<3>(value);
            }
        }
        if (!found) {
            flatProfile.push_back(value);
        }
    }

    std::unordered_map<std::wstring, std::wstring> filenameIndex;

    size_t idx = 0;
    for (auto& k : flatProfile) {
        std::wstring filename = utf8_to_wstring(std::get<0>(k));
        auto it = filenameIndex.find(filename);
        if (it == filenameIndex.end()) {
            std::wstring destination
                = profileDirectory + L"/file" + std::to_wstring(idx) + L".html";
            filenameIndex.insert(std::make_pair(filename, destination));
            idx++;
        }
    }

    std::vector<std::tuple<std::wstring, std::wstring, int, double, double>> indexData;

    for (const std::pair<std::wstring, std::wstring>& element : filenameIndex) {
        stringVector functionContent = readFunction(element.first);
        std::tuple<int, double> res
            = computeBasicFileStats(flatProfile, functionContent, element.first);

        std::vector<std::tuple<int, std::string, int, double>> fiveSlowerLines
            = getFiveLinesConsumingMostTime(flatProfile, element.first, functionContent);

        std::tuple<int, int, int, int, int, double> coverage
            = coverageAnalyzer(flatProfile, element.first, functionContent);

        indexData.emplace_back(element.first, element.second, std::get<0>(res), std::get<1>(res),
            std::get<5>(coverage));

        std::vector<std::tuple<int, double>> lineInfo
            = getInfoForContent(flatProfile, element.first, functionContent.size());

        generateProfileFileHtml(element.first, functionContent, fiveSlowerLines, coverage, lineInfo,
            std::get<0>(res), std::get<1>(res), element.second);
    }
    generateProfileIndexHtml(profileDirectory + L"/index.html", indexData);
    copyHtmlDependencies(moduleProfilerPath, profileDirectory);
}
//=============================================================================
static int
findFunctionDefinitonLine(const stringVector& functionContent)
{
    for (size_t k = 0; k < functionContent.size(); ++k) {
        std::string line = StringHelpers::trim_copy(functionContent[k]);
        if (StringHelpers::starts_with(line, "function ")) {
            return (int)k;
        }
    }
    return -1;
}
//=============================================================================
static int
findEndfunctionDefinitonLine(const stringVector& functionContent, int start)
{
    for (size_t k = (size_t)start + 1; k < functionContent.size(); ++k) {
        std::string line = StringHelpers::trim_copy(functionContent[k]);
        if (StringHelpers::starts_with(line, "endfunction")
            || StringHelpers::starts_with(line, "function")) {
            return (int)k;
        }
    }
    return -1;
}
//=============================================================================
std::tuple<int, double>
Profiler::computeBasicFileStats(
    const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const stringVector& functionContent, const std::wstring& srcFilename)
{
    double totalTime = 0;
    int nbCalls = 0;
    if (findFunctionDefinitonLine(functionContent) == -1) {
        size_t nbLines = functionContent.size();
        for (size_t k = 1; k < nbLines + 1; ++k) {
            double t;
            int n;
            if (getInfoForLine(flatProfile, srcFilename, k, n, t)) {
                nbCalls = n;
                totalTime = totalTime + t;
            }
        }
    } else {
        int start = findFunctionDefinitonLine(functionContent);
        int stop = findEndfunctionDefinitonLine(functionContent, start);
        if (stop == -1) {
            stop = (int)functionContent.size();
        }
        bool first = true;
        for (size_t k = start + 1; k < stop; ++k) {
            double t;
            int n;
            if (getInfoForLine(flatProfile, srcFilename, k + 1, n, t)) {
                if (first) {
                    nbCalls = n;
                    first = false;
                }
                totalTime = totalTime + t;
            }
        }
    }
    return std::make_tuple(nbCalls, totalTime);
}
//=============================================================================
std::vector<std::tuple<int, std::string, int, double>>
Profiler::getFiveLinesConsumingMostTime(
    const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const std::wstring& srcFilename, const stringVector& functionContent)
{
    std::vector<std::tuple<int, std::string, int, double>> lines;

    for (const auto& k : flatProfile) {
        if (utf8_to_wstring(std::get<0>(k)) == srcFilename) {
            int linePos = (int)std::get<1>(k);
            double time = std::get<3>(k) * 1e-9;

            std::string lineContent;
            if ((linePos - 1) < functionContent.size()) {
                lineContent = functionContent[linePos - 1];
                std::tuple<int, std::string, int, double> line
                    = std::make_tuple(linePos, lineContent, (int)std::get<2>(k), time);
                lines.push_back(line);
            }
        }
    }
    int nth = std::min(5, (int)lines.size());
    std::nth_element(lines.begin(), lines.begin() + nth, lines.end(),
        [](const std::tuple<int, std::string, int, double>& lhs,
            const std::tuple<int, std::string, int, double>& rhs) {
            return std::get<3>(lhs) > std::get<3>(rhs);
        });
    lines.resize(nth);
    return lines;
}
//=============================================================================
static bool
isKeyWordWithMaybeComments(std::string key)
{
    size_t index = key.find('%', 0);
    if (index == std::string::npos) {
        index = key.length();
    }
    std::string cleanKey;
    if (index > 0) {
        cleanKey = key.substr(0, index);
    } else {
        cleanKey = key;
    }
    return cleanKey == "endfunction" || cleanKey == "try" || cleanKey == "catch"
        || cleanKey == "else" || cleanKey == "end"
        || StringHelpers::starts_with(cleanKey, "function ")
        || StringHelpers::starts_with(cleanKey, "case ")
        || StringHelpers::starts_with(cleanKey, "otherwise ")
        || StringHelpers::starts_with(cleanKey, "break");
}
//=============================================================================
std::tuple<int, int, int, int, int, double>
Profiler::coverageAnalyzer(
    const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const std::wstring& srcFilename, const stringVector& functionContent)
{
    int totalLines = (int)functionContent.size();
    int nonCodeLines = 0;
    int linesCanRun = 0;
    int linesDidRun = 0;
    int linesDidNotRun = 0;
    double coverage = 0.;

    int keywords = 0;

    for (const std::string& line : functionContent) {
        std::string temp = StringHelpers::trim_copy(line);
        if (temp.empty() || StringHelpers::starts_with(temp, "%")) {
            nonCodeLines++;
        } else if (isKeyWordWithMaybeComments(temp)) {
            keywords++;
        }
    }
    linesCanRun = totalLines - nonCodeLines - keywords;
    std::vector<std::tuple<uint64, uint64, uint64>> linesInfo
        = getProfileForFile(flatProfile, srcFilename);
    linesDidRun = (int)linesInfo.size();
    linesDidNotRun = linesCanRun - linesDidRun;
    coverage = (double(linesDidRun) / double(linesCanRun)) * 100.;
    return std::make_tuple(
        totalLines, nonCodeLines, linesCanRun, linesDidRun, linesDidNotRun, coverage);
}
//=============================================================================
std::vector<std::tuple<uint64, uint64, uint64>>
Profiler::getProfileForFile(
    const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const std::wstring& srcFilename)
{
    std::vector<std::tuple<uint64, uint64, uint64>> profileByLine;
    for (const auto& k : flatProfile) {
        if (utf8_to_wstring(std::get<0>(k)) == srcFilename) {
            profileByLine.emplace_back(std::get<1>(k), std::get<2>(k), std::get<3>(k));
        }
    }
    return profileByLine;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
