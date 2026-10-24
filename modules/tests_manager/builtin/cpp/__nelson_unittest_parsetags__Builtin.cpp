//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__nelson_unittest_parsetags__Builtin.hpp"
#include "Error.hpp"
#include "ParseTags.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include <atomic>
#include <thread>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
static wstringVector
fieldNames();
//=============================================================================
static std::vector<bool>
tagValues(TestTags& tags);
//=============================================================================
static ArrayOf
logicalCell(const std::vector<std::vector<bool>>& parsedValues, size_t fieldIndex);
//=============================================================================
static ArrayOf
weightCell(std::vector<TestTags>& parsedTags);
//=============================================================================
static ArrayOf
tagsStruct(std::vector<TestTags>& parsedTags);
//=============================================================================
static wstringVector
filenamesFromInput(const ArrayOf& input);
//=============================================================================
static void
parseFiles(
    const wstringVector& filenames, std::vector<TestTags>& parsedTags, wstringVector& messages);
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_parsetags__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    wstringVector filenames = filenamesFromInput(argIn[0]);
    std::vector<TestTags> parsedTags;
    wstringVector messages;
    parseFiles(filenames, parsedTags, messages);
    for (size_t index = 0; index < filenames.size(); ++index) {
        if (!messages[index].empty()) {
            RaiseError(L"File " + std::to_wstring(index + 1) + L" ('" + filenames[index] + L"'): "
                    + messages[index],
                L"Nelson:tests_manager:parseTagsFailed");
        }
    }
    retval << tagsStruct(parsedTags);
    return retval;
}
//=============================================================================
namespace {
//=============================================================================
static wstringVector
fieldNames()
{
    return { L"not_fixed", L"interactive_test", L"with_display", L"cli_mode", L"gui_mode",
        L"adv_cli_mode", L"check_ref", L"english_imposed", L"windows_only", L"mac_only",
        L"unix_only", L"release_only", L"excel_required", L"mpi_mode", L"audio_input_required",
        L"audio_output_required", L"audio_required", L"c_cpp_compiler_required",
        L"index_64_bit_required", L"no_user_modules", L"ipc_required", L"sequential_test_required",
        L"native_architecture_required", L"file_watcher_required", L"python_environment_required",
        L"julia_environment_required", L"reuse_process", L"weight", L"timeout" };
}
//=============================================================================
static std::vector<bool>
tagValues(TestTags& tags)
{
    return { tags.isNotFixed(), tags.isInteractiveTest(), tags.isWithDisplay(), tags.isCliMode(),
        tags.isGuiMode(), tags.isAdvCliMode(), tags.isCheckRef(), tags.isEnglishImposed(),
        tags.isWindowsOnly(), tags.isMacOnly(), tags.isUnixOnly(), tags.isReleaseOnly(),
        tags.isExcelRequired(), tags.isMpiMode(), tags.isAudioInputRequired(),
        tags.isAudioOutputRequired(), tags.isAudioRequired(), tags.isCCompilerRequired(),
        tags.isIndex64BitRequired(), tags.isNoUserModules(), tags.isIpcRequired(),
        tags.isSequentialTestRequired(), tags.isNativeArchitecturedRequired(),
        tags.isFileWatcherRequired(), tags.isPythonEnvironmentRequired(),
        tags.isJuliaEnvironmentRequired(), tags.isReuseProcess() };
}
//=============================================================================
static ArrayOf
logicalCell(const std::vector<std::vector<bool>>& parsedValues, size_t fieldIndex)
{
    ArrayOfVector row;
    row.reserve(parsedValues.size());
    for (const std::vector<bool>& values : parsedValues) {
        row.push_back(ArrayOf::logicalScalar(values[fieldIndex]));
    }
    ArrayOfMatrix matrix;
    matrix.push_back(row);
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
static ArrayOf
weightCell(std::vector<TestTags>& parsedTags)
{
    ArrayOfVector row;
    row.reserve(parsedTags.size());
    for (TestTags& tags : parsedTags) {
        row.push_back(ArrayOf::doubleScalar(static_cast<double>(tags.getWeight())));
    }
    ArrayOfMatrix matrix;
    matrix.push_back(row);
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
static ArrayOf
timeoutCell(std::vector<TestTags>& parsedTags)
{
    ArrayOfVector row;
    row.reserve(parsedTags.size());
    for (TestTags& tags : parsedTags) {
        row.push_back(ArrayOf::doubleScalar(static_cast<double>(tags.getTimeout())));
    }
    ArrayOfMatrix matrix;
    matrix.push_back(row);
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
static ArrayOf
tagsStruct(std::vector<TestTags>& parsedTags)
{
    wstringVector names = fieldNames();
    if (parsedTags.empty()) {
        Dimensions dims;
        dims[0] = 0;
        dims[1] = 0;
        return ArrayOf::emptyStruct(names, dims);
    }
    std::vector<std::vector<bool>> parsedValues;
    parsedValues.reserve(parsedTags.size());
    for (TestTags& tags : parsedTags) {
        parsedValues.push_back(tagValues(tags));
    }
    if (parsedTags.size() == 1) {
        ArrayOfVector fields;
        fields.reserve(parsedValues[0].size());
        for (bool value : parsedValues[0]) {
            fields.push_back(ArrayOf::logicalScalar(value));
        }
        fields.push_back(ArrayOf::doubleScalar(static_cast<double>(parsedTags[0].getWeight())));
        fields.push_back(ArrayOf::doubleScalar(static_cast<double>(parsedTags[0].getTimeout())));
        return ArrayOf::structArray(names, fields);
    }
    ArrayOfVector fields;
    fields.reserve(names.size());
    for (size_t fieldIndex = 0; fieldIndex < names.size() - 2; ++fieldIndex) {
        fields.push_back(logicalCell(parsedValues, fieldIndex));
    }
    fields.push_back(weightCell(parsedTags));
    fields.push_back(timeoutCell(parsedTags));
    return ArrayOf::structArray(names, fields);
}
//=============================================================================
static wstringVector
filenamesFromInput(const ArrayOf& input)
{
    if (input.isRowVectorCharacterArray() || input.isScalarStringArray()) {
        return { input.asWideString() };
    }
    return input.asWideStringVector(true);
}
//=============================================================================
static void
parseFiles(
    const wstringVector& filenames, std::vector<TestTags>& parsedTags, wstringVector& messages)
{
    parsedTags.resize(filenames.size());
    messages.resize(filenames.size());
    constexpr size_t parallelThreshold = 64;
    size_t hardwareThreads = static_cast<size_t>(std::thread::hardware_concurrency());
    size_t workerCount = filenames.size() < parallelThreshold
        ? 1
        : (std::min)(filenames.size(),
              (std::min)(hardwareThreads == 0 ? 1 : hardwareThreads, static_cast<size_t>(8)));
    if (workerCount == 1) {
        for (size_t index = 0; index < filenames.size(); ++index) {
            ParseTags(filenames[index], parsedTags[index], messages[index]);
        }
        return;
    }
    std::atomic_size_t nextIndex { 0 };
    std::vector<std::thread> workers;
    workers.reserve(workerCount);
    for (size_t worker = 0; worker < workerCount; ++worker) {
        workers.emplace_back([&]() {
            size_t index;
            while ((index = nextIndex.fetch_add(1)) < filenames.size()) {
                ParseTags(filenames[index], parsedTags[index], messages[index]);
            }
        });
    }
    for (std::thread& worker : workers) {
        worker.join();
    }
}
//=============================================================================
} // namespace
//=============================================================================
