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
// Next numeric argument decoded for an integer / character conversion.
struct PrintfNumeric
{
    bool isEmpty = false; // the source argument was empty (produces no output)
    bool integerValued = false; // finite and represents an exact integer
    bool fitsInt64 = false; // integer value in the signed 64-bit range
    bool fitsUint64 = false; // integer value in the unsigned 64-bit range
    double dv = 0.0; // value as double (real part for complex)
    long long i64 = 0; // exact signed value when fitsInt64
    unsigned long long u64 = 0; // exact unsigned value when fitsUint64
};
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
    void
    AdvanceToNextArgument();
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
    GetNextNumeric(PrintfNumeric& out, std::wstring& errorMessage);
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
