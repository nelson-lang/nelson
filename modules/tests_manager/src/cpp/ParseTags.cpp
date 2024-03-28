//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include "ParseTags.hpp"
#include "Comments.hpp"
#include "FileParts.hpp"
#include "FileSystemWrapper.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isEmptyLine(const std::string& line)
{
    std::string str = StringHelpers::trim_left_copy(line);
    return str.empty();
}
//=============================================================================
static bool
compareTag(const std::string& line, const std::string& tag)
{
    std::string modifiedLine = StringHelpers::trim_left_copy(line);
    bool wasComment = false;
    if (StringHelpers::starts_with(modifiedLine, "%")) {
        StringHelpers::replace_first(modifiedLine, "%", "");
        wasComment = true;
    }
    if (wasComment) {
        StringHelpers::trim(modifiedLine);
        if (!modifiedLine.empty()) {
            if (StringHelpers::starts_with(modifiedLine, PREFIX_TAG)
                && StringHelpers::ends_with(modifiedLine, POSTFIX_TAG)) {
                StringHelpers::replace_first(modifiedLine, PREFIX_TAG, "");
                StringHelpers::replace_last(modifiedLine, POSTFIX_TAG, "");
                StringHelpers::trim(modifiedLine);
                std::string cleanedTag = StringHelpers::trim_copy(tag);
                StringHelpers::replace_first(cleanedTag, PREFIX_TAG, "");
                StringHelpers::replace_last(cleanedTag, POSTFIX_TAG, "");
                StringHelpers::trim(cleanedTag);
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
    if (!FileSystemWrapper::Path::is_regular_file(filename, permissionDenied)) {
        if (permissionDenied) {
            msg = _W("Permission denied.");
        } else {
            msg = _W("an existing file expected.");
        }
        return false;
    }
    std::wstring basename = FilePartsFilename(filename);
    std::wstring ext = FilePartsExtension(filename);
    bool isValidFilename = (StringHelpers::starts_with(basename, L"test_")
                               || StringHelpers::starts_with(basename, L"bench_")
                               || StringHelpers::starts_with(basename, L"bug_"))
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
        std::vector<std::string> tags = { WITH_DISPLAY_TAG, NOT_FIXED_TAG, INTERACTIVE_TEST_TAG,
            ENGLISH_IMPOSED_TAG, CLI_MODE_TAG, ADV_CLI_MODE_TAG, GUI_MODE_TAG, CHECK_REF_TAG,
            WINDOWS_ONLY_TAG, MACOS_ONLY_TAG, UNIX_ONLY_TAG, RELEASE_ONLY_TAG, EXCEL_REQUIRED_TAG,
            IPC_REQUIRED_TAG, FILE_WATCHER_REQUIRED_TAG, MPI_MODE_TAG, AUDIO_INPUT_REQUIRED_TAG,
            AUDIO_OUTPUT_REQUIRED_TAG, C_COMPILER_REQUIRED_TAG, INDEX_64_BIT_REQUIRED_TAG,
            NO_USER_MODULES_TAG, SEQUENTIAL_TEST_REQUIRED_TAG, NATIVE_ARCHITECTURE_REQUIRED_TAG,
            PYTHON_ENVIRONMENT_REQUIRED_TAG };

        std::unordered_map<std::string, bool> firstTagOccurrences;
        std::unordered_map<std::string, bool> tagOptions;

        for (const std::string& tag : tags) {
            firstTagOccurrences[tag] = true;
            tagOptions[tag] = false;
        }

        std::string line;
        while (std::getline(istream, line)) {
            if (!isCommentedLine(line) && !isEmptyLine(line)) {
                istream.close();
                break;
            }
            for (const std::string& tag : tags) {
                if (compareTag(line, tag) && firstTagOccurrences[tag]) {
                    if (tagOptions[tag]) {
                        msg = fmt::sprintf(_W("Duplicated tag detected: %s"), utf8_to_wstring(tag));
                        istream.close();
                        return false;
                    }
                    tagOptions[tag] = true;
                    firstTagOccurrences[tag] = false;

                    if (tag == WITH_DISPLAY_TAG) {
                        options.setWithDisplay(true);
                    } else if (tag == NOT_FIXED_TAG) {
                        options.setNotFixed(true);
                    } else if (tag == INTERACTIVE_TEST_TAG) {
                        options.setInteractiveTest(true);
                    } else if (tag == ENGLISH_IMPOSED_TAG) {
                        options.setEnglishImposed(true);
                    } else if (tag == CLI_MODE_TAG) {
                        options.setCliMode(true);
                        options.setAdvCliMode(false);
                        options.setGuiMode(false);
                    } else if (tag == ADV_CLI_MODE_TAG) {
                        options.setCliMode(false);
                        options.setAdvCliMode(true);
                        options.setGuiMode(false);
                    } else if (tag == GUI_MODE_TAG) {
                        options.setCliMode(false);
                        options.setAdvCliMode(false);
                        options.setGuiMode(true);
                    } else if (tag == CHECK_REF_TAG) {
                        options.setCheckRef(true);
                    } else if (tag == WINDOWS_ONLY_TAG) {
                        options.setWindowsOnly(true);
                    } else if (tag == MACOS_ONLY_TAG) {
                        options.setMacOnly(true);
                    } else if (tag == UNIX_ONLY_TAG) {
                        options.setUnixOnly(true);
                    } else if (tag == RELEASE_ONLY_TAG) {
                        options.setReleaseOnly(true);
                    } else if (tag == EXCEL_REQUIRED_TAG) {
                        options.setExcelRequired(true);
                    } else if (tag == IPC_REQUIRED_TAG) {
                        options.setIpcRequired(true);
                    } else if (tag == FILE_WATCHER_REQUIRED_TAG) {
                        options.setFileWatcherRequired(true);
                    } else if (tag == PYTHON_ENVIRONMENT_REQUIRED_TAG) {
                        options.setPythonEnvironmentRequired(true);
                    } else if (tag == MPI_MODE_TAG) {
                        options.setMpiMode(true);
                    } else if (tag == AUDIO_INPUT_REQUIRED_TAG) {
                        options.setAudioInputRequired(true);
                    } else if (tag == AUDIO_OUTPUT_REQUIRED_TAG) {
                        options.setAudioOutputRequired(true);
                    } else if (tag == C_COMPILER_REQUIRED_TAG) {
                        options.setCCompilerRequired(true);
                    } else if (tag == INDEX_64_BIT_REQUIRED_TAG) {
                        options.setIndex64BitRequired(true);
                    } else if (tag == NO_USER_MODULES_TAG) {
                        options.setNoUserModules(true);
                    } else if (tag == SEQUENTIAL_TEST_REQUIRED_TAG) {
                        options.setSequentialTestRequired(true);
                    } else if (tag == NATIVE_ARCHITECTURE_REQUIRED_TAG) {
                        options.setNativeArchitecturedRequired(true);
                    } else {
                        break;
                    }
                }
            }
        }
        istream.close();
    }

    int countMode = 0;
    if (options.isGuiMode()) {
        countMode++;
    }
    if (options.isAdvCliMode()) {
        countMode++;
    }
    if (options.isCliMode()) {
        countMode++;
    }
    if (countMode > 1) {
        msg = _W("Check tags used.");
        return false;
    }

    if (options.isGuiMode() || options.isAdvCliMode()) {
        options.setWithDisplay(true);
    }
    if (!options.isGuiMode() && !options.isAdvCliMode()) {
        options.setCliMode(true);
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
