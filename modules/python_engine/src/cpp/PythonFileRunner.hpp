//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Types.hpp"
#include "PythonLibraryWrapper.hpp"
#include <string>
#include <mutex>
#include <condition_variable>
//=============================================================================
namespace Nelson {
//=============================================================================
class PythonFileRunner
{
public:
    //=============================================================================
    void
    runFile(const std::wstring& filename, const wstringVector& arguments);
    //=============================================================================
    bool
    isResultReady();
    //=============================================================================
    std::wstring
    getErrorMessage();
    //=============================================================================
    std::wstring
    getStandardOutput();
    //=============================================================================
private:
    std::mutex resultMutex;
    std::condition_variable resultCondition;
    std::wstring errorMessage;
    std::wstring outputMessage;
    bool resultReady = false;
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
