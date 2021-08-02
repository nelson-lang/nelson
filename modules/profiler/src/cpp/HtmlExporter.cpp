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
#include <iostream>
#include <fstream>
#include <boost/algorithm/string/trim.hpp>
#include <boost/chrono/chrono.hpp>
#include <boost/date_time.hpp>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include "HtmlExporter.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
toStringWithPrecision(double value, int precision)
{
    std::ostringstream stream;
    stream << std::fixed;
    stream << std::setprecision(precision);
    stream << value;
    return stream.str();
}
//=============================================================================
static std::string
timeToString(double value)
{
    if (value < 0.001) {
        return "&lt; 0.001";
    }
    return toStringWithPrecision(value, 3);
}
//=============================================================================
bool
copyHtmlDependencies(
    const std::wstring& moduleProfilerPath, const std::wstring& directoryDestination)
{
    if (!moduleProfilerPath.empty()) {
        std::wstring ressourcesPath = moduleProfilerPath + L"/resources/";
        wstringVector files;
        files.push_back(L"highlight.pack.js");
        files.push_back(L"sort.js");
        files.push_back(L"mono-blue.css");
        for (size_t k = 0; k < files.size(); k++) {
            boost::filesystem::path dstFile = directoryDestination;
            dstFile = dstFile / files[k];
            if (!boost::filesystem::exists(dstFile)) {
                boost::filesystem::path srcFile = ressourcesPath;
                srcFile = srcFile / files[k];
                bool bIsFile = boost::filesystem::exists(srcFile)
                    && !boost::filesystem::is_directory(srcFile);
                if (bIsFile) {
                    boost::filesystem::copy_file(srcFile, dstFile);
                }
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
void
generateProfileIndexHtml(const std::wstring& htmlFilename,
    std::vector<std::tuple<std::wstring, std::wstring, int, double, double>> indexData)
{
#ifdef _MSC_VER
    std::ofstream file(htmlFilename);
#else
    std::ofstream file(wstring_to_utf8(htmlFilename));
#endif
    file << "<html lang=\"en\">" << std::endl;
    file << "<head>" << std::endl;
    file << "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">"
         << std::endl;
    file << "    <title>Profile and Coverage " << _("Summary") << "</title>" << std::endl;
    file << "</head>" << std::endl;
    file << "<body bgcolor=#F8F8F8>" << std::endl;

    file << "<p></p>" << std::endl;
    file << "<b>" << _("Profile Summary") << "</b>" << std::endl;
    file << "<p></p>" << std::endl;

    file << "<div>" << std::endl;
    file << "<table id = \"indextable\" border = \"1\" cellpadding = \"10\" cellspacing = \"0\" "
            "style = \"border-collapse:collapse;\"><thead><tr>"
         << std::endl;
    file << "<th><a href = \"javascript:SortTable(0,'T');\">" << _("Filename") << "</a></th>"
         << std::endl;
    file << "<th><a href = \"javascript:SortTable(1,'N');\">" << _("Number of Calls") << "</a></th>"
         << std::endl;
    file << "<th><a href = \"javascript:SortTable(2,'N');\">" << _("Total Time (s)") << "</a></th>"
         << std::endl;
    file << "<th><a href = \"javascript:SortTable(2,'N');\">" << _("Coverage %") << "</a></th>"
         << std::endl;

    file << "</tr></thead><tbody>" << std::endl;

    for (std::tuple<std::wstring, std::wstring, int, double, double> element : indexData) {

        double totalTime = std::get<3>(element);
        int nbCalls = std::get<2>(element);
        double coverage = std::get<4>(element);
        boost::filesystem::path p1(std::get<1>(element));
        std::string file_x_html = wstring_to_utf8(p1.filename().wstring());
        boost::filesystem::path p2(std::get<0>(element));
        std::string filename = wstring_to_utf8(p2.wstring());

        file << "<tr>" << std::endl;
        file << "<td><a href = \"" << file_x_html << "\">" << filename
             << "</a></td><td align=\"right\">" << nbCalls << "</td><td align=\"right\">"
             << timeToString(totalTime) << "</td>"
             << "<td>" << toStringWithPrecision(coverage, 2) << "</td>" << std::endl;
        file << "</tr>" << std::endl;
        file << std::endl;
    }

    file << "</tbody></table>" << std::endl;
    file << "<script src = \"sort.js\"></script>" << std::endl;
    file << "</div>" << std::endl;
    file << "<p></p>" << std::endl;
    file << "<hr>" << std::endl;
    boost::posix_time::ptime currentDateTime = boost::posix_time::second_clock::local_time();
    file << "<tr><td class=\"versionInfo\">Generated by: Nelson " << currentDateTime << "</td></tr>"
         << std::endl;

    file << "</body></html>" << std::endl;

    file.close();
}
//=============================================================================
static void
sectionMostTimeWasSpent(
    std::ofstream& file, std::vector<std::tuple<int, std::string, int, double>> fiveSlowerLines)
{
    if (!fiveSlowerLines.empty()) {
        file << "<hr>" << std::endl;
        file << "<p>" << _("Lines where the most time was spent") << "</p>" << std::endl;
        file << "<table cellpadding = \"0\" cellspacing = \"0\" border = \"1\">" << std::endl;
        file << "<tbody>" << std::endl;
        file << "<tr>" << std::endl;
        file << "<td style = \"padding-left:10px;padding-right:10px;\">" << _("Line Number")
             << "</td>" << std::endl;
        file << "<td style = \"padding-left:10px;padding-right:10px;\">" << _("Code") << "</td>"
             << std::endl;
        file << "<td style = \"padding-left:10px;padding-right:10px;\">" << _("Calls") << "</td>"
             << std::endl;
        file << "<td style = \"padding-left:10px;padding-right:10px;\">" << _("Total time (s)")
             << "</td>" << std::endl;
        file << "</tr>" << std::endl;
        file << "</tbody>" << std::endl;
    }
    for (size_t k = 0; k < fiveSlowerLines.size(); ++k) {
        file << "<tbody>" << std::endl;
        file << "<tr>" << std::endl;
        file << "<td align=\"right\" style = \"padding-left:10px;padding-right:10px;\"><a name = \""
             << std::to_string(std::get<0>(fiveSlowerLines[k])) << "\"></a>";
        file << "<span class = \"lineNum\">" << std::to_string(std::get<0>(fiveSlowerLines[k]))
             << "</span></td>" << std::endl;
        std::string content = std::get<1>(fiveSlowerLines[k]);
        if (content.empty()) {
            content = "&nbsp;";
        }
        file << "<td><pre><code class = \"nelson\">" << boost::trim_left_copy(content)
             << "</code></pre></td>";

        file << "<td style = \"padding-left:10px;padding-right:10px;\">"
             << std::to_string(std::get<2>(fiveSlowerLines[k])) << "</td>" << std::endl;
        file << "<td style = \"padding-left:10px;padding-right:10px;\">"
             << timeToString(std::get<3>(fiveSlowerLines[k])) << "</td>" << std::endl;
        file << "</tr>" << std::endl;
        file << "</tbody>" << std::endl;
    }
    file << "</table>" << std::endl;
}
//=============================================================================
static void
sectionCoverage(std::ofstream& file, std::tuple<int, int, int, int, int, double> coverage)
{
    file << "<p>" << _("Coverage results") << "</p>" << std::endl;

    file << "<table cellpadding = \"0\" cellspacing = \"0\" border = \"1\">" << std::endl;
    file << "<tbody>" << std::endl;
    file << "<tr>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">" << _("Total lines in file")
         << "</td>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">" << std::get<0>(coverage)
         << "</td>" << std::endl;
    file << "</tr>" << std::endl;

    file << "<tr>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">"
         << _("Non-code lines (comments, blank lines)") << "</td>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">" << std::get<1>(coverage)
         << "</td>" << std::endl;
    file << "</tr>" << std::endl;

    file << "<tr>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">" << _("Code lines that did run")
         << "</td>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">" << std::get<2>(coverage)
         << "</td>" << std::endl;
    file << "</tr>" << std::endl;

    file << "<tr>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">"
         << _("Code lines (lines that can run)") << "</td>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">" << std::get<3>(coverage)
         << "</td>" << std::endl;
    file << "</tr>" << std::endl;

    file << "<tr>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">"
         << _("Code lines (Code lines that did not run)") << "</td>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">" << std::get<4>(coverage)
         << "</td>" << std::endl;
    file << "</tr>" << std::endl;

    file << "<tr>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">"
         << _("Coverage (did run/can run)") << "</td>" << std::endl;
    file << "<td style = \"padding-left:10px;padding-right:10px;\">"
         << toStringWithPrecision(std::get<5>(coverage), 2) << " %"
         << "</td>" << std::endl;
    file << "</tr>" << std::endl;

    file << "</tbody></table>" << std::endl;
}
//=============================================================================
static void
sectionFunctionListing(std::ofstream& file, const stringVector& functionContent,
    std::vector<std::tuple<int, double>> lineInfo)
{
    file << "<p>" << _("Function listing") << "</p>" << std::endl;
    file << "<table cellpadding = \"0\" cellspacing = \"0\" border = \"0\">" << std::endl;
    file << "    <thead>" << std::endl;
    file << "        <tr>" << std::endl;
    file << "            <td  style = \"padding-left:10px;padding-right:10px;\">" << _("Line")
         << "</td>" << std::endl;
    file << "            <td  style = \"padding-left:10px;padding-right:10px;\">" << _("Calls")
         << "</td>" << std::endl;
    file << "            <td  style = \"padding-left:10px;padding-right:10px;\">" << _("Time (s)")
         << "</td>" << std::endl;
    file << "            <td  style = \"padding-left:10px;padding-right:10px;\">"
         << _("Source code") << "</td>" << std::endl;
    file << "        </tr>" << std::endl;
    file << "    </thead>" << std::endl;
    file << "    <tbody>" << std::endl;

    size_t i = 0;
    for (size_t i = 0; i < lineInfo.size(); i++) {
        std::string lineAsStr = std::to_string(i + 1);

        int numcalls = std::get<0>(lineInfo[i]);
        double time = std::get<1>(lineInfo[i]);
        std::string numAsStr;
        std::string timeAsStr;

        if (numcalls == -1) {
            numAsStr.clear();
        } else {
            numAsStr = std::to_string(numcalls);
        }
        if (time < 0) {
            timeAsStr.clear();
        } else {
            timeAsStr = timeToString(time);
        }

        file << "        <tr>" << std::endl;
        file << "            <td align = \"right\" style = "
                "\"padding-left:10px;padding-right:10px;\"><a name = "
                "\""
             << lineAsStr << "\"><span class = \"lineNum\">" << lineAsStr << "</span></td>"
             << std::endl;
        file << "            <td align = \"right\" style = "
                "\"padding-left:10px;padding-right:10px;\">"
             << numAsStr << "</td>" << std::endl;
        file << "            <td align = \"right\" style = "
                "\"padding-left:10px;padding-right:10px;\">"
             << timeAsStr << "</td>" << std::endl;
        std::string content = functionContent[i];
        if (content.empty()) {
            content = "&nbsp;";
        }

        file << "            <td style = \"padding-left:10px;padding-right:10px;\"><pre><code "
                "class = "
                "\"nelson\">"
             << content
             << "</"
                "code></pre></td>"
             << std::endl;
        file << "        </tr>" << std::endl;
    }
    file << "    </tbody>" << std::endl;
    file << "</table>" << std::endl;
}
//=============================================================================
void
generateProfileFileHtml(const std::wstring& srcFilename, const stringVector& functionContent,
    const std::vector<std::tuple<int, std::string, int, double>>& fiveSlowerLines,
    std::tuple<int, int, int, int, int, double> coverage,
    std::vector<std::tuple<int, double>> lineInfo, int nbCalls, double totalTime,
    const std::wstring& htmlFilename)
{
#ifdef _MSC_VER
    std::ofstream file(htmlFilename);
#else
    std::ofstream file(wstring_to_utf8(htmlFilename));
#endif
    size_t nbLines = functionContent.size();
    size_t lenNbLines = std::max(std::to_string(nbLines).size(), _("lines").size() + 1);
    file << "<html lang=\"en\">" << std::endl;
    file << "<head>" << std::endl;
    file << "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">"
         << std::endl;
    file << "    <title>" << _("Profile and Coverage") << " " << wstring_to_utf8(srcFilename)
         << "</title>" << std::endl;
    file << "</head>" << std::endl;
    file << "" << std::endl;

    file << "<link rel = \"stylesheet\" href = \"./mono-blue.css\">" << std::endl;
    file << "<script src = "
            "\"./highlight.pack.js\"></script>"
         << std::endl;
    file << "<script>hljs.initHighlightingOnLoad();</script>" << std::endl;

    file << "<body bgcolor=#F8F8F8>" << std::endl;
    file << "<div>" << std::endl;
    file << "<a href=\""
         << "./"
         << "index.html"
         << "\">"
         << "home"
         << "</a>" << std::endl;
    file << "</div>" << std::endl;
    file << "<p></p>" << std::endl;
    file << "<p>" << _("File") << ": " << wstring_to_utf8(srcFilename) << " (" << _("Calls") << ": "
         << std::to_string(nbCalls) << ", " << _("Time") << ": " << timeToString(totalTime) << " s"
         << ")"
         << "</p>" << std::endl;
    file << "<p></p>" << std::endl;

    sectionMostTimeWasSpent(file, fiveSlowerLines);
    file << "<p></p>" << std::endl;
    file << "<hr>" << std::endl;

    sectionCoverage(file, coverage);
    file << "<p></p>" << std::endl;
    file << "<hr>" << std::endl;

    sectionFunctionListing(file, functionContent, lineInfo);
    file << "<p></p>" << std::endl;
    file << "<hr>" << std::endl;

    boost::posix_time::ptime currentDateTime = boost::posix_time::second_clock::local_time();
    file << "<tr><td class=\"versionInfo\">Generated by: Nelson " << currentDateTime << "</td></tr>"
         << std::endl;
    file << "<p></p>" << std::endl;
    file << "<div>" << std::endl;
    file << "<a href=\""
         << "./"
         << "index.html"
         << "\">"
         << "home"
         << "</a>" << std::endl;
    file << "</div>" << std::endl;

    file << "<p></p>" << std::endl;

    file << "</body></html>" << std::endl;

    file.close();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
