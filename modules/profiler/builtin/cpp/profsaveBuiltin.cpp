//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "profsaveBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Profiler.hpp"
#include "ModulesManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// profsave
// profsave(profinfo)
// profsave(profinfo, dirname)
//=============================================================================
static std::wstring
filePartPath(const std::wstring& dirname)
{
    FileSystemWrapper::Path pathToSplit(dirname);
    std::wstring path;
    if (pathToSplit.has_parent_path()) {
        path = pathToSplit.parent_path().generic_wstring();
    }
    return path;
}
//=============================================================================
ArrayOfVector
Nelson::ProfilerGateway::profsaveBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 0, 2);
    std::wstring fullDirname;
    if (argIn.size() == 2) {
        std::wstring dirname = argIn[1].asWideString();
        FileSystemWrapper::Path pathToSplit(dirname);
        if (filePartPath(dirname).empty()) {
            std::wstring currentDirname = FileSystemWrapper::Path::current_path().generic_wstring();
            fullDirname = currentDirname + L"/" + dirname;
        } else {
            fullDirname = dirname;
        }
    } else {
        std::wstring currentDirname = FileSystemWrapper::Path::current_path().generic_wstring();
        fullDirname = currentDirname + L"/profile_result";
    }

    std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>> profilerInfo;

    if (argIn.size() == 0) {
        Profiler::Profile_Sort_Type sortOption = Profiler::Profile_Sort_Type::SORT_BY_NAMEFILELINE;
        profilerInfo = Profiler::getInstance()->info(sortOption);
    } else {
        ArrayOf param1 = argIn[0];
        if (!param1.isStruct()) {
            Error(L"Nelson:profsave:invalidProfileStruct", L"Profile struct expected.");
        }
        stringVector fieldnames = param1.fieldNames();
        stringVector expectedNames;
        expectedNames.push_back("FunctionName");
        expectedNames.push_back("Filename");
        expectedNames.push_back("LinePosition");
        expectedNames.push_back("NumCalls");
        expectedNames.push_back("TotalTime");
        expectedNames.push_back("PerCall");
        if (fieldnames.size() != expectedNames.size()) {
            Error(L"Nelson:profsave:invalidProfileStruct", L"Profile struct expected.");
        }
        for (size_t index = 0; index < expectedNames.size(); ++index) {
            if (fieldnames[index] != expectedNames[index]) {
                Error(L"Nelson:profsave:invalidProfileStruct", L"Profile struct expected.");
            }
        }
        ArrayOfVector functionnames = param1.fieldAsList("FunctionName");
        ArrayOfVector filenames = param1.fieldAsList("Filename");
        ArrayOfVector positions = param1.fieldAsList("LinePosition");
        ArrayOfVector numcalls = param1.fieldAsList("NumCalls");
        ArrayOfVector totaltimes = param1.fieldAsList("TotalTime");
        ArrayOfVector percalls = param1.fieldAsList("PerCall");
        for (size_t k = 0; k < filenames.size(); ++k) {
            std::string functionname = functionnames[k].asUtf8String();
            std::string filename = filenames[k].asUtf8String();
            uint64 position = (uint64)positions[k].asDoubleScalar();
            uint64 numcall = (uint64)numcalls[k].asDoubleScalar();
            uint64 totaltime = (uint64)(totaltimes[k].asDoubleScalar() * 1e9);
            uint64 percall = (uint64)(percalls[k].asDoubleScalar() * 1e9);
            profilerInfo.emplace_back(
                filename, position, functionname, numcall, totaltime, percall);
        }
    }
    std::wstring errorMessage;
    Profiler::getInstance()->save(
        profilerInfo, fullDirname, GetModulePath(L"profiler"), errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:profsave:saveFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
