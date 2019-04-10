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
#include <boost/chrono/chrono.hpp>
#include <boost/filesystem.hpp>
#include <iomanip>
#include <tuple>
#include "Profiler.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
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
sortByPerCall(const std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 percallA = std::get<6>(a);
    uint64 percallB = std::get<6>(b);

    if (percallA > percallB) {
        return true;
    }
    return false;
}
//=============================================================================
static bool
sortByNbCalls(const std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 nbcallsA = std::get<4>(a);
    uint64 nbcallsB = std::get<4>(b);
    if (nbcallsA > nbcallsB) {
        return true;
    }
    return false;
}
//=============================================================================
static bool
sortByFilename(
    const std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string filenameA = std::get<1>(a);
    std::string filenameB = std::get<1>(b);
    if (filenameA.compare(filenameB) > 0) {
        return true;
    }
    return false;
}
//=============================================================================
static bool
sortByLine(const std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 lineposA = std::get<2>(a);
    uint64 lineposB = std::get<2>(b);

    return lineposA < lineposB;
}
//=============================================================================
static bool
sortByName(const std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string nameA = std::get<3>(a);
    std::string nameB = std::get<3>(b);
    if (nameA.compare(nameB) > 0) {
        return true;
    }
    return false;
}
//=============================================================================
static bool
sortByTotalTime(
    const std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 tottimeA = std::get<5>(a);
    uint64 tottimeB = std::get<5>(b);
    if (tottimeA > tottimeB) {
        return true;
    }
    return false;
}
//=============================================================================
static bool
sortByNameFilenameLine(
    const std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string nameA = std::get<3>(a);
    std::string nameB = std::get<3>(b);
    uint64 lineposA = std::get<2>(a);
    uint64 lineposB = std::get<2>(b);
    std::string filenameA = std::get<1>(a);
    std::string filenameB = std::get<1>(b);

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
std::vector<std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>>
Profiler::info(Profiler::Profile_Sort_Type sortOption)
{
    std::vector<std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>>
        profilerLines;

    for (std::pair<size_t, profileFunction> element : profileMap) {
        profileFunction pF = element.second;
        uint64 index = std::get<3>(pF);
        internalProfileFunction iPf = std::get<0>(pF);
        profileParentStack parentStack = std::get<0>(iPf);
        std::string name = std::get<1>(iPf);
        profileParent lastParent = parentStack[parentStack.size() - 1];
        int lastLinePosition = (int)std::get<1>(lastParent);
        std::string lastFunctionName = std::get<0>(lastParent);
        int k = (int)parentStack.size();
        while (lastLinePosition == 0 && k - 2 >= 0) {
            k = k - 2;
            lastParent = parentStack[k];
            lastLinePosition = (int)std::get<1>(lastParent);
            lastFunctionName = std::get<0>(lastParent);
        }

        uint64 nbcalls = std::get<1>(pF);
        uint64 tottime = std::get<2>(pF);
        uint64 line = lastLinePosition;
        boost::filesystem::path p(lastFunctionName);
        std::string filename = p.filename().string();
        uint64 percall = tottime / nbcalls;
        profilerLines.push_back(
            std::make_tuple(index, filename, line, name, nbcalls, tottime, percall));
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
Profiler::show(Interface* io, Profiler::Profile_Sort_Type sortOption)
{
    // index, filename, line, name, nbcalls, tottime, percall
    std::vector<std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64>>
        profilerLines = Profiler::info(sortOption);

    std::stringstream ssLine;
    ssLine << std::endl;
    ssLine << std::right << std::setfill(' ') << std::setw(10) << _("NumCalls");
    ssLine << std::right << std::setfill(' ') << std::setw(16) << _("TotalTime (s)");
    ssLine << std::right << std::setfill(' ') << std::setw(16) << _("PerCall (s)");
    ssLine << "    " << std::right << std::setfill(' ') << std::setw(16)
           << _("Filename:LinePosition(FunctionName)");
    ssLine << std::endl;
    ssLine << std::endl;
    io->outputMessage(ssLine.str());

    for (std::tuple<uint64, std::string, uint64, std::string, uint64, uint64, uint64> line :
        profilerLines) {
        // index, filename, line, name, nbcalls, tottime, percall
        uint64 index = std::get<0>(line);
        std::string filename = std::get<1>(line);
        uint64 linepos = std::get<2>(line);
        std::string name = std::get<3>(line);
        uint64 nbcalls = std::get<4>(line);
        uint64 tottime = std::get<5>(line);
        uint64 percall = std::get<6>(line);
        std::stringstream ssLine;
        std::string flf = filename + ":" + std::to_string((int)linepos) + "(" + name + ")";

        ssLine << std::right << std::setfill(' ') << std::setw(10)
               << std::to_string((int)nbcalls);
        ssLine << std::right << std::setfill(' ') << std::setw(16)
               << std::to_string(tottime * 1e-9);
        ssLine << std::right << std::setfill(' ') << std::setw(16)
               << std::to_string(percall * 1e-9);
        ssLine << "    " << std::right << std::setfill(' ') << std::setw(16) << flf;
        ssLine << std::endl;
        io->outputMessage(ssLine.str());
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
