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
#include <boost/algorithm/string.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include "ParseTags.hpp"
#include "Comments.hpp"
#include "FileParts.hpp"
#include "IsFile.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "StringFormat.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isEmptyLine(const std::string& line)
{
    std::string str = boost::algorithm::trim_left_copy(line);
    return str.empty();
}
//=============================================================================
static bool
compareTag(const std::string& line, const std::string& tag)
{
    std::string modifiedLine = boost::algorithm::trim_left_copy(line);
    bool wasComment = false;
    if (boost::algorithm::starts_with(modifiedLine, "//")) {
        boost::algorithm::replace_first(modifiedLine, "//", "");
        wasComment = true;
    } else if (boost::algorithm::starts_with(modifiedLine, "#")) {
        boost::algorithm::replace_first(modifiedLine, "#", "");
        wasComment = true;
    }
    if (wasComment) {
        boost::algorithm::trim(modifiedLine);
        if (!modifiedLine.empty()) {
            if (boost::algorithm::starts_with(modifiedLine, PREFIX_TAG)
                && boost::algorithm::ends_with(modifiedLine, POSTFIX_TAG)) {
                boost::algorithm::replace_first(modifiedLine, PREFIX_TAG, "");
                boost::algorithm::replace_last(modifiedLine, POSTFIX_TAG, "");
                boost::algorithm::trim(modifiedLine);
                std::string cleanedTag = boost::algorithm::trim_copy(tag);
                boost::algorithm::replace_first(cleanedTag, PREFIX_TAG, "");
                boost::algorithm::replace_last(cleanedTag, POSTFIX_TAG, "");
                boost::algorithm::trim(cleanedTag);
                return (modifiedLine.compare(cleanedTag) == 0);
            }
        }
    }
    return false;
}
//=============================================================================
bool
ParseTags(const std::wstring& filename, TestTags& options, std::wstring& msg)
{
    if (!IsFile(filename)) {
        msg = _W("an existing file expected.");
        return false;
    }
    std::wstring basename = FilePartsFilename(filename);
    std::wstring ext = FilePartsExtension(filename);
    bool isValidFilename = (boost::algorithm::starts_with(basename, L"test_")
                               || boost::algorithm::starts_with(basename, L"bench_")
                               || boost::algorithm::starts_with(basename, L"bug_"))
        && (ext == L".nls");
    if (!isValidFilename) {
        if (ext != L".nls") {
            msg = _W("wrong file extension .nls expected.");
            return false;
        }
        msg = _W("wrong file prefix 'test_' or 'bug_' expected.");
        return false;
    }
    std::ifstream istream;
#ifdef _MSC_VER
    istream.open(filename);
#else
    istream.open(wstring_to_utf8(filename));
#endif
    if (istream.is_open()) {
        bool firstCliTag = true;
        bool firstAdvCliTag = true;
        bool firstGuiTag = true;
        bool firstNotFixedTag = true;
        bool firstInteractiveTag = true;
        bool firstCheckRefTag = true;
        bool firstEnglishTag = true;
        bool firstWindowsTag = true;
        bool firstMacTag = true;
        bool firstLinuxTag = true;
        bool firstWithDisplayTag = true;
        bool firstReleaseOnlyTag = true;
        bool firstExcelRequiredTag = true;
        bool firstMpiModeTag = true;
        bool firstAudioInputRequiredTag = true;
        bool firstAudioOutputRequiredTag = true;
        bool firstCCompilerRequiredTag = true;
        bool firstIndex64BitRequiredTag = true;
        bool firstNoUserModules = true;
        std::string line;
        while (!istream.eof()) {
            std::getline(istream, line);
            if (!isCommentedLine(line) && (!isEmptyLine(line))) {
                istream.close();
                break;
            }
            if (compareTag(line, WITH_DISPLAY_TAG) && firstWithDisplayTag) {
                if (options.isWithDisplay() && !firstWithDisplayTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(WITH_DISPLAY_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setWithDisplay(true);
                firstWithDisplayTag = false;
                continue;
            }
            if (compareTag(line, NOT_FIXED_TAG) && firstNotFixedTag) {
                if (options.isNotFixed() && !firstNotFixedTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(NOT_FIXED_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setNotFixed(true);
                firstNotFixedTag = false;
                continue;
            }
            if (compareTag(line, INTERACTIVE_TEST_TAG) && firstInteractiveTag) {
                if (options.isInteractiveTest() && !firstInteractiveTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(INTERACTIVE_TEST_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setInteractiveTest(true);
                firstInteractiveTag = false;
                continue;
            }
            if (compareTag(line, ENGLISH_IMPOSED_TAG) && firstEnglishTag) {
                if (options.isEnglishImposed() && !firstEnglishTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(ENGLISH_IMPOSED_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setEnglishImposed(true);
                firstEnglishTag = false;
                continue;
            }
            if (compareTag(line, CLI_MODE_TAG) && firstCliTag) {
                if ((options.isAdvCliMode() && !firstAdvCliTag)
                    || (options.isGuiMode() && !firstGuiTag)) {
                    msg = _W("Check tags used.");
                    istream.close();
                    return false;
                }
                if (options.isCliMode() && !firstCliTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(CLI_MODE_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setCliMode(true);
                options.setAdvCliMode(false);
                options.setGuiMode(false);
                firstCliTag = false;
                continue;
            }
            if (compareTag(line, ADV_CLI_MODE_TAG) && firstAdvCliTag) {
                if ((options.isCliMode() && !firstCliTag)
                    || (options.isGuiMode() && !firstGuiTag)) {
                    msg = _W("Check tags used.");
                    istream.close();
                    return false;
                }
                if (options.isAdvCliMode() && firstAdvCliTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(ADV_CLI_MODE_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setAdvCliMode(true);
                options.setCliMode(false);
                options.setGuiMode(false);
                firstAdvCliTag = false;
                continue;
            }
            if (compareTag(line, GUI_MODE_TAG) && firstGuiTag) {
                if ((options.isCliMode() && !firstCliTag)
                    || (options.isAdvCliMode() && !firstAdvCliTag)) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(GUI_MODE_TAG).c_str());
                    istream.close();
                    return false;
                }
                if (options.isGuiMode() && firstGuiTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(GUI_MODE_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setGuiMode(true);
                options.setAdvCliMode(false);
                options.setCliMode(false);
                firstGuiTag = false;
                continue;
            }
            if (compareTag(line, CHECK_REF_TAG) && firstCheckRefTag) {
                if (options.isCheckRef() && !firstAdvCliTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(CHECK_REF_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setCheckRef(true);
                firstCheckRefTag = false;
                continue;
            }
            if (compareTag(line, WINDOWS_ONLY_TAG) && firstWindowsTag) {
                if ((options.isUnixOnly() && !firstLinuxTag)
                    || (options.isMacOnly() && !firstMacTag)) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(WINDOWS_ONLY_TAG).c_str());
                    istream.close();
                    return false;
                }
                if (options.isWindowsOnly() && !firstWindowsTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(WINDOWS_ONLY_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setWindowsOnly(true);
                firstWindowsTag = false;
                continue;
            }
            if (compareTag(line, MACOS_ONLY_TAG) && firstMacTag) {
                if ((options.isUnixOnly() && !firstLinuxTag)
                    || (options.isWindowsOnly() && !firstWindowsTag)) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(MACOS_ONLY_TAG).c_str());
                    istream.close();
                    return false;
                }
                if (options.isMacOnly() && !firstMacTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(MACOS_ONLY_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setMacOnly(true);
                firstMacTag = false;
                continue;
            }
            if (compareTag(line, UNIX_ONLY_TAG) && firstLinuxTag) {
                if ((options.isMacOnly() && !firstMacTag)
                    || (options.isWindowsOnly() && !firstWindowsTag)) {
                    msg = StringFormat(_W("Multiple exclusive tags detected: %ls").c_str(),
                        utf8_to_wstring(UNIX_ONLY_TAG).c_str());
                    istream.close();
                    return false;
                }
                if (options.isUnixOnly() && !firstLinuxTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(UNIX_ONLY_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setUnixOnly(true);
                firstLinuxTag = false;
                continue;
            }
            if (compareTag(line, RELEASE_ONLY_TAG) && firstReleaseOnlyTag) {
                if (!firstReleaseOnlyTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(RELEASE_ONLY_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setReleaseOnly(true);
                firstReleaseOnlyTag = false;
                continue;
            }
            if (compareTag(line, EXCEL_REQUIRED_TAG) && firstExcelRequiredTag) {
                if (!firstExcelRequiredTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(EXCEL_REQUIRED_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setExcelRequired(true);
                firstExcelRequiredTag = false;
                continue;
            }
            if (compareTag(line, MPI_MODE_TAG) && firstMpiModeTag) {
                if (!firstMpiModeTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(MPI_MODE_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setMpiMode(true);
                options.setCliMode(true);
                firstCliTag = false;
                firstMpiModeTag = false;
                continue;
            }
            if (compareTag(line, AUDIO_INPUT_REQUIRED_TAG) && firstAudioInputRequiredTag) {
                if (!firstAudioInputRequiredTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(AUDIO_INPUT_REQUIRED_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setAudioInputRequired(true);
                firstAudioInputRequiredTag = false;
                continue;
            }
            if (compareTag(line, AUDIO_OUTPUT_REQUIRED_TAG) && firstAudioOutputRequiredTag) {
                if (!firstAudioOutputRequiredTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(AUDIO_OUTPUT_REQUIRED_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setAudioOutputRequired(true);
                firstAudioOutputRequiredTag = false;
                continue;
            }
            if (compareTag(line, C_COMPILER_REQUIRED_TAG) && firstCCompilerRequiredTag) {
                if (!firstCCompilerRequiredTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(C_COMPILER_REQUIRED_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setCCompilerRequired(true);
                firstCCompilerRequiredTag = false;
                continue;
            }
            if (compareTag(line, INDEX_64_BIT_REQUIRED_TAG) && firstIndex64BitRequiredTag) {
                if (!firstIndex64BitRequiredTag) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(INDEX_64_BIT_REQUIRED_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setIndex64BitRequired(true);
                firstIndex64BitRequiredTag = false;
                continue;
            }
            if (compareTag(line, NO_USER_MODULES_TAG) && firstNoUserModules) {
                if (!firstNoUserModules) {
                    msg = StringFormat(_W("Duplicated tag detected: %ls").c_str(),
                        utf8_to_wstring(NO_USER_MODULES_TAG).c_str());
                    istream.close();
                    return false;
                }
                options.setNoUserModules(true);
                firstNoUserModules = false;
                continue;
            }
        }
        istream.close();
        if (options.isGuiMode() || options.isAdvCliMode()) {
            options.setWithDisplay(true);
        }
        if (!options.isGuiMode() && !options.isAdvCliMode()) {
            options.setCliMode(true);
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
