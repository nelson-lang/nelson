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
#include <boost/date_time.hpp>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include <iomanip>
#include <tuple>
#include <iostream>
#include <fstream>
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
sortByPerCall(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 percallA = std::get<5>(a);
    uint64 percallB = std::get<5>(b);

    return percallA < percallB;
}
//=============================================================================
static bool
sortByNbCalls(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 nbcallsA = std::get<3>(a);
    uint64 nbcallsB = std::get<3>(b);
    return nbcallsA < nbcallsB;
}
//=============================================================================
static bool
sortByFilename(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string filenameA = std::get<0>(a);
    std::string filenameB = std::get<0>(b);

    return filenameA.compare(filenameB) < 0;
}
//=============================================================================
static bool
sortByLine(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 lineposA = std::get<1>(a);
    uint64 lineposB = std::get<1>(b);

    return lineposA < lineposB;
}
//=============================================================================
static bool
sortByName(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    std::string nameA = std::get<2>(a);
    std::string nameB = std::get<2>(b);
    return nameA.compare(nameB) < 0;
}
//=============================================================================
static bool
sortByTotalTime(const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
{
    uint64 tottimeA = std::get<4>(a);
    uint64 tottimeB = std::get<4>(b);
    return tottimeA < tottimeB;
}
//=============================================================================
static bool
sortByNameFilenameLine(
    const std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& a,
    std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>& b)
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
Profiler::show(Interface* io, Profiler::Profile_Sort_Type sortOption)
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

    for (std::tuple<std::string, uint64, std::string, uint64, uint64, uint64> line :
        profilerLines) {
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

        ssLine << std::right << std::setfill(' ') << std::setw(10) << std::to_string((int)nbcalls);
        ssLine << std::right << std::setfill(' ') << std::setw(16)
               << std::to_string(tottime * 1e-9);
        ssLine << std::right << std::setfill(' ') << std::setw(16)
               << std::to_string(percall * 1e-9);
        ssLine << "    " << std::left << std::setfill(' ') << std::setw(24) << flf;
        ssLine << std::endl;
        io->outputMessage(ssLine.str());
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
static bool
getInfoByLine(const std::vector<std::tuple<std::string, uint64, uint64, uint64>>& flatProfile,
    const std::string& filename, size_t line, int& numcalls, double& time)
{
    for (size_t k = 0; k < flatProfile.size(); ++k) {
        if (std::get<0>(flatProfile[k]) == filename && std::get<1>(flatProfile[k]) == line) {
            numcalls = (int)std::get<2>(flatProfile[k]);
            time = std::get<3>(flatProfile[k]) * 1e-9;
            return true;
        }
    }
    return false;
}
//=============================================================================
void
Profiler::save(const std::wstring& destinationDirectory, std::wstring& errorMessage)
{
    std::wstring profileDirectory = destinationDirectory + L"/profile_results";
    try {
        if (!boost::filesystem::exists(profileDirectory)) {
            boost::filesystem::create_directory(profileDirectory);
        }
    } catch (const boost::filesystem::filesystem_error& e) {
        boost::system::error_code error_code = e.code();
        errorMessage = utf8_to_wstring(error_code.message());
        return;
    }

    Profiler::Profile_Sort_Type sortOption = Profiler::Profile_Sort_Type::SORT_BY_NAMEFILELINE;

    std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>> profilerLines
        = Profiler::info(sortOption);

    // filename, line, time, calls
    std::vector<std::tuple<std::string, uint64, uint64, uint64>> flatProfile;
    // filename, line, name, nbcalls, tottime, percall
    for (std::tuple<std::string, uint64, std::string, uint64, uint64, uint64> element :
        profilerLines) {
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

    std::unordered_map<std::string, std::wstring> filenameIndex;

    int idx = 0;
    for (size_t k = 0; k < flatProfile.size(); ++k) {
        std::string filename = std::get<0>(flatProfile[k]);
        auto it = filenameIndex.find(filename);
        if (it == filenameIndex.end()) {
            std::wstring destination
                = profileDirectory + L"/file" + std::to_wstring(idx) + L".html";
            filenameIndex.insert(std::make_pair(filename, destination));
            idx++;
        }
    }

    for (std::pair<std::string, std::wstring> element : filenameIndex) {
        stringVector functionContent;
#ifdef _MSC_VER
        std::ifstream in(utf8_to_wstring(element.first));
#else
        std::ifstream in(element.first);
#endif
        std::string str;
        while (std::getline(in, str)) {
            functionContent.push_back(str);
        }

#ifdef _MSC_VER
        std::ofstream file(element.second);
#else
        std::ofstream file(wstring_to_utf8(element.second));
#endif
        size_t nbLines = functionContent.size();
        size_t lenNbLines = std::max(std::to_string(nbLines).size(), _("lines").size() + 1);

        file << "<html lang=\"en\">" << std::endl;
        file << "<head>" << std::endl;
        file << "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">"
             << std::endl;
        file << "    <title>Profile and Coverage d:/profile_tests/test_2.nlf</title>" << std::endl;
        file << "</head>" << std::endl;
        file << "<body>" << std::endl;
        file << "<table cellpadding=\"0\" cellspacing=\"0\" border=\"0\">" << std::endl;
        file << "<tbody><tr>" << std::endl;
        file << "<td><br></td>" << std::endl;
        file << "</tr>" << std::endl;
        file << "<tr>" << std::endl;
        file << "<td>" << std::endl;
        file << "<pre class=\"sourceHeading\">"
             << " Line :   NumCalls :      Time : Source code"
             << "</pre>" << std::endl;
        file << "<pre class=\"source\">" << std::endl;

        for (size_t i = 0; i < nbLines; i++) {
            int numcalls = 0;
            double time = 0.;
            std::string numAsStr;
            std::string timeAsStr;
            std::string lineAsStr = std::to_string(i + 1);

            if (getInfoByLine(flatProfile, element.first, i + 1, numcalls, time)) {
                numAsStr = std::to_string(numcalls);
                timeAsStr = std::to_string(time);
            } else {
                numAsStr = "";
                timeAsStr = "";
            }
            file << "<a name = \"" << lineAsStr << "\"><span class = \"lineNum\">"
                 << std::string(lenNbLines - lineAsStr.size(), ' ') << lineAsStr << "</span>"
                 << " : " << std::string(10 - numAsStr.size(), ' ') << numAsStr << " : "
                 << std::string(10 - timeAsStr.size(), ' ') << timeAsStr << " : "
                 << functionContent[i] << "</a>" << std::endl;
        }
        file << "</pre>" << std::endl;
        file << "</td>" << std::endl;
        file << "</tr>" << std::endl;
        file << "</tbody></table>" << std::endl;
        file << "<br>" << std::endl;
        file << "<table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">"
             << std::endl;
        boost::posix_time::ptime currentDateTime = boost::posix_time::second_clock::local_time();

        file << "<tr><td class=\"versionInfo\">Generated by: Nelson " << currentDateTime
             << "</td></tr>" << std::endl;
        file << "</tbody></table>" << std::endl;
        file << "<br>" << std::endl;
        file << "</body></html>" << std::endl;

        file.close();
    }

    std::wstring index_html = profileDirectory + L"/index.html";
#ifdef _MSC_VER
    std::ofstream file(index_html);
#else
    std::ofstream file(wstring_to_utf8(index_html));
#endif
    for (std::pair<std::string, std::wstring> element : filenameIndex) {
        boost::filesystem::path p1(element.second);
        std::string file_x_html = p1.filename().string();
        boost::filesystem::path p2(element.first);
        std::string filename = p2.filename().string();

        file << "<div>" << std::endl;
        file << "<a href=\""
             << "./" << file_x_html << "\">" << filename << "</a>" << std::endl;
        file << "</div>" << std::endl;
    }
    file.close();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
