//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
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
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
Display2dStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplayStruct(Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        DisplayEmptyStruct(io, A, name, currentNumericFormat, currentLineSpacing);
    } else if (A.isScalar()) {
        DisplayScalarStruct(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.is2D()) {
        Display2dStruct(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        DisplayNdStruct(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
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
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    stringVector fieldnames = A.getFieldNames();
    size_t maxLen = 0;
    for (const auto& fieldname : fieldnames) {
        maxLen = std::max(fieldname.length(), maxLen);
    }
    if (!fieldnames.empty()) {
        if (!name.empty()) {
            io->outputMessage(L"\n");
        }
        ArrayOf* ap = static_cast<ArrayOf*>(
            const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        for (size_t k = 0; k < fieldnames.size(); ++k) {
            std::wstring beginning = BLANKS_AT_BOL
                + completeWithBlanksAtBeginning(utf8_to_wstring(fieldnames[k]), maxLen) + L": ";
            std::wstring valueAsString = summarizeCellEntry(ap[k], 0, io->getTerminalWidth(),
                NelsonConfiguration::getInstance()->getNumericFormatDisplay(), false);
            io->outputMessage(beginning + valueAsString + L"\n");
        }
    }
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
Display2dStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
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
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
DisplayNdStruct(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
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
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
