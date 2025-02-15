//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "test_parsetagsBuiltin.hpp"
#include "Error.hpp"
#include "ParseTags.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::test_parsetagsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    TestTags tags;
    ArrayOf param1 = argIn[0];
    std::wstring value = param1.getContentAsWideString();
    std::wstring msg;
    if (!ParseTags(value, tags, msg)) {
        Error(msg);
    }
    size_t nbFields = 65;
    wstringVector fieldnames;
    fieldnames.reserve(nbFields);
    ArrayOfVector fieldvalues;
    fieldvalues.reserve(nbFields);
    fieldnames.push_back(L"not_fixed");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isNotFixed()));
    fieldnames.push_back(L"interactive_test");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isInteractiveTest()));
    fieldnames.push_back(L"with_display");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isWithDisplay()));
    fieldnames.push_back(L"cli_mode");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isCliMode()));
    fieldnames.push_back(L"gui_mode");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isGuiMode()));
    fieldnames.push_back(L"adv_cli_mode");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isAdvCliMode()));
    fieldnames.push_back(L"check_ref");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isCheckRef()));
    fieldnames.push_back(L"english_imposed");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isEnglishImposed()));
    fieldnames.push_back(L"windows_only");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isWindowsOnly()));
    fieldnames.push_back(L"mac_only");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isMacOnly()));
    fieldnames.push_back(L"unix_only");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isUnixOnly()));
    fieldnames.push_back(L"release_only");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isReleaseOnly()));
    fieldnames.push_back(L"excel_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isExcelRequired()));
    fieldnames.push_back(L"mpi_mode");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isMpiMode()));
    fieldnames.push_back(L"audio_input_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isAudioInputRequired()));
    fieldnames.push_back(L"audio_output_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isAudioOutputRequired()));
    fieldnames.push_back(L"c_cpp_compiler_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isCCompilerRequired()));
    fieldnames.push_back(L"index_64_bit_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isIndex64BitRequired()));
    fieldnames.push_back(L"no_user_modules");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isNoUserModules()));
    fieldnames.push_back(L"ipc_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isIpcRequired()));
    fieldnames.push_back(L"sequential_test_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isSequentialTestRequired()));
    fieldnames.push_back(L"native_architecture_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isNativeArchitecturedRequired()));
    fieldnames.push_back(L"file_watcher_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isFileWatcherRequired()));
    fieldnames.push_back(L"python_environment_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isPythonEnvironmentRequired()));
    fieldnames.push_back(L"julia_environment_required");
    fieldvalues.push_back(ArrayOf::logicalConstructor(tags.isJuliaEnvironmentRequired()));

    retval << ArrayOf::structConstructor(fieldnames, fieldvalues);
    return retval;
}
//=============================================================================
