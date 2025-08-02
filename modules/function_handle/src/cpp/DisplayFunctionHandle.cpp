//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DisplayFunctionHandle.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
DisplayScalarFunctionHandle(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplayFunctionHandle(Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isScalar()) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE
            && !asDisp) {
            io->outputMessage(L"\n");
        }
        DisplayScalarFunctionHandle(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        Error(_W("Case not managed. Please report."));
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayScalarFunctionHandle(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{

    function_handle fh = A.getContentAsFunctionHandle();
    AnonymousMacroFunctionDef* anonymousFunction
        = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
    std::string content = anonymousFunction->getDefinition();
    std::wstring formatedContent = BLANKS_AT_BOL + utf8_to_wstring(content) + L"\n";
    io->outputMessage(formatedContent);
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
