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
    int vectorIndex;
    int elementIndex;
    bool hasMoreData;
    bool dataUsed;
    void
    IncrementDataPointer(void);
    static int
    flagCharacter(wchar_t c);
    static int
    convSpec(wchar_t c);
    static std::wstring
    ConvertEscapeSequences(const std::wstring& src);

public:
    PrintfHelper(const ArrayOfVector& arg_);
    bool
    GetNextVariableAsDouble(double& data, std::wstring& errorMessage, bool& isEmpty);
    bool
    GetNextVariableAsLongLong(long long& data, std::wstring& errorMessage, bool& isEmpty);
    bool
    GetNextVariableAsString(std::wstring& str, std::wstring& errorMessage);
    bool
    HasMoreData(void);
    bool
    WasDataUsed(void);
    static bool
    isEscape(wchar_t* dp);
    static wchar_t*
    validateFormatSpec(wchar_t* cp);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
