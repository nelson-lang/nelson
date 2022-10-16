//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <boost/algorithm/string.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include "ParseTags.hpp"
#include "Comments.hpp"
#include "FileParts.hpp"
#include "FileSystemHelpers.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
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
    if (boost::algorithm::starts_with(modifiedLine, "%")) {
        boost::algorithm::replace_first(modifiedLine, "%", "");
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
    bool permissionDenied;
    if (!isFile(filename, permissionDenied)) {
        if (permissionDenied) {
            msg = _W("Permission denied.");
        } else {
            msg = _W("an existing file expected.");
        }
        return false;
    }
    std::wstring basename = FilePartsFilename(filename);
    std::wstring ext = FilePartsExtension(filename);
    bool isValidFilename = (boost::algorithm::starts_with(basename, L"test_")
                               || boost::algorithm::starts_with(basename, L"bench_")
                               || boost::algorithm::starts_with(basename, L"bug_"))
        && (ext == L".m");
    if (!isValidFilename) {
        bool isSupportedFileExtension = (ext == L".m");
        if (!isSupportedFileExtension) {
            msg = _W("wrong file extension .m or .m expected.");
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
        bool firstIpcRequired = true;
        bool firstSequentialTestRequired = true;
        bool firstNativeArchitectureTestRequired = true;
        std::string line;
        while (!istream.eof()) {
            std::getline(istream, line);
            if (!isCommentedLine(line) && (!isEmptyLine(line))) {
                istream.close();
                break;
            }
            if (compareTag(line, WITH_DISPLAY_TAG) && firstWithDisplayTag) {
                if (options.isWithDisplay() && !firstWithDisplayTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(WITH_DISPLAY_TAG));
                    istream.close();
                    return false;
                }
                options.setWithDisplay(true);
                firstWithDisplayTag = false;
                continue;
            }
            if (compareTag(line, NOT_FIXED_TAG) && firstNotFixedTag) {
                if (options.isNotFixed() && !firstNotFixedTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(NOT_FIXED_TAG));
                    istream.close();
                    return false;
                }
                options.setNotFixed(true);
                firstNotFixedTag = false;
                continue;
            }
            if (compareTag(line, INTERACTIVE_TEST_TAG) && firstInteractiveTag) {
                if (options.isInteractiveTest() && !firstInteractiveTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(INTERACTIVE_TEST_TAG));
                    istream.close();
                    return false;
                }
                options.setInteractiveTest(true);
                firstInteractiveTag = false;
                continue;
            }
            if (compareTag(line, ENGLISH_IMPOSED_TAG) && firstEnglishTag) {
                if (options.isEnglishImposed() && !firstEnglishTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(ENGLISH_IMPOSED_TAG));
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
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(CLI_MODE_TAG));
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
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(ADV_CLI_MODE_TAG));
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
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(GUI_MODE_TAG));
                    istream.close();
                    return false;
                }
                if (options.isGuiMode() && firstGuiTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(GUI_MODE_TAG));
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
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(CHECK_REF_TAG));
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
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(WINDOWS_ONLY_TAG));
                    istream.close();
                    return false;
                }
                if (options.isWindowsOnly() && !firstWindowsTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(WINDOWS_ONLY_TAG));
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
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(MACOS_ONLY_TAG));
                    istream.close();
                    return false;
                }
                if (options.isMacOnly() && !firstMacTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(MACOS_ONLY_TAG));
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
                    msg = fmt::sprintf(
                        _W("Multiple exclusive tags detected: %s"), utf8_to_wstring(UNIX_ONLY_TAG));
                    istream.close();
                    return false;
                }
                if (options.isUnixOnly() && !firstLinuxTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(UNIX_ONLY_TAG));
                    istream.close();
                    return false;
                }
                options.setUnixOnly(true);
                firstLinuxTag = false;
                continue;
            }
            if (compareTag(line, RELEASE_ONLY_TAG) && firstReleaseOnlyTag) {
                if (!firstReleaseOnlyTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(RELEASE_ONLY_TAG));
                    istream.close();
                    return false;
                }
                options.setReleaseOnly(true);
                firstReleaseOnlyTag = false;
                continue;
            }
            if (compareTag(line, EXCEL_REQUIRED_TAG) && firstExcelRequiredTag) {
                if (!firstExcelRequiredTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(EXCEL_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setExcelRequired(true);
                firstExcelRequiredTag = false;
                continue;
            }
            if (compareTag(line, IPC_REQUIRED_TAG) && firstIpcRequired) {
                if (!firstIpcRequired) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(IPC_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setIpcRequired(true);
                firstIpcRequired = false;
                continue;
            }

            if (compareTag(line, MPI_MODE_TAG) && firstMpiModeTag) {
                if (!firstMpiModeTag) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(MPI_MODE_TAG));
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
                    msg = fmt::sprintf(_W("Duplicated tag detected: %s"),
                        utf8_to_wstring(AUDIO_INPUT_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setAudioInputRequired(true);
                firstAudioInputRequiredTag = false;
                continue;
            }
            if (compareTag(line, AUDIO_OUTPUT_REQUIRED_TAG) && firstAudioOutputRequiredTag) {
                if (!firstAudioOutputRequiredTag) {
                    msg = fmt::sprintf(_W("Duplicated tag detected: %s"),
                        utf8_to_wstring(AUDIO_OUTPUT_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setAudioOutputRequired(true);
                firstAudioOutputRequiredTag = false;
                continue;
            }
            if (compareTag(line, C_COMPILER_REQUIRED_TAG) && firstCCompilerRequiredTag) {
                if (!firstCCompilerRequiredTag) {
                    msg = fmt::sprintf(_W("Duplicated tag detected: %s"),
                        utf8_to_wstring(C_COMPILER_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setCCompilerRequired(true);
                firstCCompilerRequiredTag = false;
                continue;
            }
            if (compareTag(line, INDEX_64_BIT_REQUIRED_TAG) && firstIndex64BitRequiredTag) {
                if (!firstIndex64BitRequiredTag) {
                    msg = fmt::sprintf(_W("Duplicated tag detected: %s"),
                        utf8_to_wstring(INDEX_64_BIT_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setIndex64BitRequired(true);
                firstIndex64BitRequiredTag = false;
                continue;
            }
            if (compareTag(line, NO_USER_MODULES_TAG) && firstNoUserModules) {
                if (!firstNoUserModules) {
                    msg = fmt::sprintf(
                        _W("Duplicated tag detected: %s"), utf8_to_wstring(NO_USER_MODULES_TAG));
                    istream.close();
                    return false;
                }
                options.setNoUserModules(true);
                firstNoUserModules = false;
                continue;
            }
            if (compareTag(line, SEQUENTIAL_TEST_REQUIRED_TAG) && firstSequentialTestRequired) {
                if (!firstSequentialTestRequired) {
                    msg = fmt::sprintf(_W("Duplicated tag detected: %s"),
                        utf8_to_wstring(SEQUENTIAL_TEST_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setSequentialTestRequired(true);
                firstSequentialTestRequired = false;
                continue;
            }
            if (compareTag(line, NATIVE_ARCHITECTURE_REQUIRED_TAG)
                && firstNativeArchitectureTestRequired) {
                if (!firstNativeArchitectureTestRequired) {
                    msg = fmt::sprintf(_W("Duplicated tag detected: %s"),
                        utf8_to_wstring(NATIVE_ARCHITECTURE_REQUIRED_TAG));
                    istream.close();
                    return false;
                }
                options.setNativeArchitecturedRequired(true);
                firstNativeArchitectureTestRequired = false;
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
