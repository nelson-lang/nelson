//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/chrono/chrono.hpp>
#include <boost/date_time.hpp>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <iomanip>
#include <tuple>
#include <iostream>
#include <fstream>
#include "Profiler.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "HtmlExporter.hpp"
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
Profiler::toc(uint64 tic, internalProfileFunction stack)
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

    for (std::pair<size_t, profileFunction> element : profileMap) {
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
            for (size_t k = 0; k < profilerLines.size(); ++k) {
                if ((std::get<0>(value) == std::get<0>(profilerLines[k]))
                    && (std::get<1>(value) == std::get<1>(profilerLines[k]))
                    && (std::get<2>(value) == std::get<2>(profilerLines[k]))) {
                    found = true;
                    std::get<3>(profilerLines[k])
                        = std::get<3>(profilerLines[k]) + std::get<3>(value);
                    std::get<4>(profilerLines[k])
                        = std::get<4>(profilerLines[k]) + std::get<4>(value);
                    std::get<5>(profilerLines[k])
                        = std::get<4>(profilerLines[k]) / std::get<3>(profilerLines[k]);
                }
            }
            if (!found) {
                profilerLines.push_back(value);
            }
        }
    }

    switch (sortOption) {
    case SORT_BY_LINE: {
        std::sort(profilerLines.begin(), profilerLines.end(), sortByLine);
    } break;
    case SORT_BY_NAMEFILELINE: {
        std::sort(profilerLines.begin(), profilerLines.end(), sortByNameFilenameLine);
    } break;
    case SORT_BY_NAME: {
        std::sort(profilerLines.begin(), profilerLines.end(), sortByName);
    } break;
    case SORT_BY_FILENAME: {
        std::sort(profilerLines.begin(), profilerLines.end(), sortByFilename);
    } break;
    case SORT_BY_NBCALLS: {
        std::sort(profilerLines.begin(), profilerLines.end(), sortByNbCalls);
    } break;
    case SORT_BY_PERCALL: {
        std::sort(profilerLines.begin(), profilerLines.end(), sortByPerCall);
    } break;
    case SORT_BY_TOTALTIME:
    default: {
        std::sort(profilerLines.begin(), profilerLines.end(), sortByTotalTime);
    } break;
    }
    return profilerLines;
}
//=============================================================================
uint64
Profiler::now()
{
    boost::chrono::nanoseconds ns = boost::chrono::high_resolution_clock::now().time_since_epoch();
    return uint64(static_cast<boost::uint64_t>(ns.count()));
}
//=============================================================================
size_t
Profiler::hash(internalProfileFunction stack)
{
    std::string res;
    profileParentStack parent = std::get<0>(stack);
    for (size_t k = 0; k < parent.size(); ++k) {
        std::string filename = std::get<0>(parent[k]);
        int linePosition = (int)std::get<1>(parent[k]);
        res = res + filename + "|" + std::to_string(linePosition) + ">";
    }
    if (res.size()) {
        res.pop_back();
    }

    res = res + "[" + wstring_to_utf8(std::get<2>(stack)) + "|" + std::get<1>(stack) + "]";
    return std::hash<std::string>{}(res);
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
            boost::filesystem::path p(std::get<0>(line));
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
static bool
isFile(const std::string& filename)
{
    boost::filesystem::path data_dir(utf8_to_wstring(filename));
    bool bRes = false;
    try {
        bRes = boost::filesystem::exists(data_dir) && !boost::filesystem::is_directory(data_dir);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            // ONLY FOR DEBUG
        }
        bRes = false;
    }
    return bRes;
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
        lines.push_back(std::make_tuple(numcalls, time));
    }
    return lines;
}
//=============================================================================
bool
Profiler::getInfoForLine(
    const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const std::wstring& filename, size_t line, int& numcalls, double& time)
{
    for (size_t k = 0; k < flatProfile.size(); ++k) {
        if (utf8_to_wstring(std::get<0>(flatProfile[k])) == filename
            && std::get<1>(flatProfile[k]) == line) {
            numcalls = (int)std::get<2>(flatProfile[k]);
            time = std::get<3>(flatProfile[k]) * 1e-9;
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
    try {
        if (!boost::filesystem::exists(profileDirectory)) {
            boost::filesystem::create_directory(profileDirectory);
        }
    } catch (const boost::filesystem::filesystem_error& e) {
        boost::system::error_code error_code = e.code();
        errorMessage = utf8_to_wstring(error_code.message());
        return;
    }

    // filename, line, time, calls
    std::vector<std::tuple<std::string, uint64, uint64, uint64>> flatProfile;
    // filename, line, name, nbcalls, tottime, percall
    for (std::tuple<std::string, uint64, std::string, uint64, uint64, uint64> element :
        profileInfo) {
        std::tuple<std::string, uint64, uint64, uint64> value = std::make_tuple(
            std::get<0>(element), std::get<1>(element), std::get<3>(element), std::get<4>(element));
        bool found = false;
        for (size_t k = 0; k < flatProfile.size(); ++k) {
            if ((std::get<0>(value) == std::get<0>(flatProfile[k]))
                && (std::get<1>(value) == std::get<1>(flatProfile[k]))) {
                found = true;
                std::get<3>(flatProfile[k]) = std::get<3>(flatProfile[k]) + std::get<3>(value);
            }
        }
        if (!found) {
            flatProfile.push_back(value);
        }
    }

    std::unordered_map<std::wstring, std::wstring> filenameIndex;

    int idx = 0;
    for (size_t k = 0; k < flatProfile.size(); ++k) {
        std::wstring filename = utf8_to_wstring(std::get<0>(flatProfile[k]));
        auto it = filenameIndex.find(filename);
        if (it == filenameIndex.end()) {
            std::wstring destination
                = profileDirectory + L"/file" + std::to_wstring(idx) + L".html";
            filenameIndex.insert(std::make_pair(filename, destination));
            idx++;
        }
    }

    std::vector<std::tuple<std::wstring, std::wstring, int, double, double>> indexData;

    for (std::pair<std::wstring, std::wstring> element : filenameIndex) {
        stringVector functionContent = readFunction(element.first);
        std::tuple<int, double> res
            = computeBasicFileStats(flatProfile, functionContent, element.first);

        std::vector<std::tuple<int, std::string, int, double>> fiveSlowerLines
            = getFiveLinesConsumingMostTime(flatProfile, element.first, functionContent);

        std::tuple<int, int, int, int, int, double> coverage
            = coverageAnalyzer(flatProfile, element.first, functionContent);

        indexData.push_back(std::make_tuple(element.first, element.second, std::get<0>(res),
            std::get<1>(res), std::get<5>(coverage)));

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
        std::string line = boost::algorithm::trim_copy(functionContent[k]);
        if (boost::algorithm::starts_with(line, "function ")) {
            return (int)k;
        }
    }
    return -1;
}
//=============================================================================
static int
findEndfunctionDefinitonLine(const stringVector& functionContent, int start)
{
    for (size_t k = start + 1; k < functionContent.size(); ++k) {
        std::string line = boost::algorithm::trim_copy(functionContent[k]);
        if (boost::algorithm::starts_with(line, "endfunction")
            || boost::algorithm::starts_with(line, "function")) {
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
    std::tuple<int, double> res;
    if (boost::algorithm::ends_with(srcFilename, L".nls")) {
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

    for (size_t k = 0; k < flatProfile.size(); ++k) {
        if (utf8_to_wstring(std::get<0>(flatProfile[k])) == srcFilename) {
            int linePos = (int)std::get<1>(flatProfile[k]);
            double time = std::get<3>(flatProfile[k]) * 1e-9;

            std::tuple<int, std::string, int, double> line = std::make_tuple(
                linePos, functionContent[linePos - 1], (int)std::get<2>(flatProfile[k]), time);
            lines.push_back(line);
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
    size_t index1 = key.find("%", 0);
    size_t index2 = key.find("//", 0);
    if (index1 == std::string::npos) {
        index1 = key.length();
    }
    if (index2 == std::string::npos) {
        index2 = key.length();
    }
    size_t index = std::min(index1, index2);
    std::string cleanKey;
    if (index > 0) {
        cleanKey = key.substr(0, index);
    } else {
        cleanKey = key;
    }
    return cleanKey == "endfunction" || cleanKey == "try" || cleanKey == "catch"
        || cleanKey == "else" || cleanKey == "end"
        || boost::algorithm::starts_with(cleanKey, "function ")
        || boost::algorithm::starts_with(cleanKey, "case ")
        || boost::algorithm::starts_with(cleanKey, "otherwise ")
        || boost::algorithm::starts_with(cleanKey, "break");
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

    size_t res = 0;
    for (std::string line : functionContent) {
        std::string temp = boost::algorithm::trim_copy(line);
        if (temp.empty() || boost::algorithm::starts_with(temp, "//")
            || boost::algorithm::starts_with(temp, "%")) {
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
    for (size_t k = 0; k < flatProfile.size(); ++k) {
        if (utf8_to_wstring(std::get<0>(flatProfile[k])) == srcFilename) {
            profileByLine.push_back(std::make_tuple(std::get<1>(flatProfile[k]),
                std::get<2>(flatProfile[k]), std::get<3>(flatProfile[k])));
        }
    }
    return profileByLine;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
