//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "DisplayStruct.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
DisplayEmptyStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayScalarStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayNdStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
void
DisplayStruct(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name);
    if (A.isEmpty()) {
        DisplayEmptyStruct(io, A, name, currentNumericFormat, currentLineSpacing);
    } else if (A.isScalar()) {
        DisplayScalarStruct(io, A, name, currentNumericFormat, currentLineSpacing);
    } else if (A.is2D()) {
        Display2dStruct(io, A, name, currentNumericFormat, currentLineSpacing);
    } else {
        DisplayNdStruct(io, A, name, currentNumericFormat, currentLineSpacing);
    }
    DisplayVariableFooter(io, A, name);
}
//=============================================================================
void
DisplayEmptyStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    stringVector fieldnames = A.getFieldNames();
    if (!fieldnames.empty()) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
            != NLS_LINE_SPACING_COMPACT) {
            io->outputMessage("\n");
        }
        for (const auto& fieldName : A.getFieldNames()) {
            io->outputMessage(BLANKS_AT_BOL);
            io->outputMessage(fieldName);
            io->outputMessage(L"\n");
        }
    }
}
//=============================================================================
void
DisplayScalarStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    if (!name.empty()) {
        io->outputMessage(L"\n");
    }
    stringVector fieldnames = A.getFieldNames();
    size_t maxLen = 0;
    for (auto name : fieldnames) {
        maxLen = std::max(name.length(), maxLen);
    }
    if (!fieldnames.empty()) {
        ArrayOf* ap = (ArrayOf*)A.getDataPointer();
        for (size_t k = 0; k < fieldnames.size(); ++k) {
            std::wstring beginning = BLANKS_AT_BOL
                + completeWithBlanksAtBeginning(utf8_to_wstring(fieldnames[k]), maxLen) + L": ";
            std::wstring valueAsString = summarizeCellEntry(
                ap[k], 0, SIZE_MAX,
                    NelsonConfiguration::getInstance()->getNumericFormatDisplay());
            io->outputMessage(beginning + valueAsString + L"\n");
        }
    }
}
//=============================================================================
void
Display2dStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    stringVector fieldnames = A.getFieldNames();
    if (!fieldnames.empty()) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
            != NLS_LINE_SPACING_COMPACT) {
            io->outputMessage("\n");
        }
        for (const auto& fieldName : A.getFieldNames()) {
            io->outputMessage(BLANKS_AT_BOL);
            io->outputMessage(fieldName);
            io->outputMessage(L"\n");
        }
    }
}
//=============================================================================
void
DisplayNdStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    stringVector fieldnames = A.getFieldNames();
    if (!fieldnames.empty()) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
            != NLS_LINE_SPACING_COMPACT) {
            io->outputMessage("\n");
        }
        for (const auto& fieldName : A.getFieldNames()) {
            io->outputMessage(BLANKS_AT_BOL);
            io->outputMessage(fieldName);
            io->outputMessage(L"\n");
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
