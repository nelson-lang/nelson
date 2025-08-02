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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class PrintfHelper
{
private:
    const ArrayOfVector args;
    int vectorIndex { 1 };
    int elementIndex { 0 };
    bool hasMoreData;
    bool dataUsed;
    void
    IncrementDataPointer();
    static int
    flagCharacter(wchar_t c);
    static int
    convSpec(wchar_t c);
    static std::wstring
    ConvertEscapeSequences(const std::wstring& src);

public:
    PrintfHelper(ArrayOfVector arg_);
    bool
    GetNextVariableAsDouble(double& data, std::wstring& errorMessage, bool& isEmpty);
    bool
    GetNextVariableAsLongLong(long long& data, std::wstring& errorMessage, bool& isEmpty);
    bool
    GetNextVariableAsString(std::wstring& str, std::wstring& errorMessage);
    bool
    HasMoreData();
    bool
    WasDataUsed();
    static bool
    isEscape(const wchar_t* dp);
    static wchar_t*
    validateFormatSpec(wchar_t* cp);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
