//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "test_parsetagsBuiltin.hpp"
#include "Error.hpp"
#include "ParseTags.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::test_parsetagsBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    TestTags tags;
    ArrayOf param1 = argIn[0];
    std::wstring value = param1.getContentAsWideString();
    std::wstring msg;
    if (!ParseTags(eval, value, tags, msg)) {
        Error(msg);
    }
    wstringVector fieldnames;
    ArrayOfVector fieldvalues;
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
    ArrayOf stack = ArrayOf::structConstructor(fieldnames, fieldvalues);
    retval.push_back(stack);
    return retval;
}
//=============================================================================
