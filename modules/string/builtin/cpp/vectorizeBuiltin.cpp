//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "vectorizeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
needsDot(wchar_t ch)
{
    return ch == L'^' || ch == L'*' || ch == L'/' || ch == L'\\';
}
//=============================================================================
std::wstring
vectorizeText(const std::wstring& input)
{
    std::wstring output;
    output.reserve(input.size() * 2);
    for (size_t k = 0; k < input.size(); ++k) {
        wchar_t ch = input[k];
        if (needsDot(ch) && (k == 0 || input[k - 1] != L'.')) {
            output.push_back(L'.');
        }
        output.push_back(ch);
    }
    return output;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::StringGateway::vectorizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::wstring text;
    if (argIn[0].isFunctionHandle()) {
        // vectorize(@(...) body) returns the vectorized source as a character vector (same as
        // vectorize(func2str(fh))), not a function handle. Callers compare it against
        // func2str(fh) to detect an already-vectorized operation.
        function_handle fh = argIn[0].asFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        if (cp == nullptr) {
            Error(L"Nelson:func2str:invalidAnonymousFunction", L"Invalid anonymous function.");
        }
        // The header '@(args)' contains no ^ * / \ operators, so vectorizing the
        // whole string only affects the body.
        text = utf8_to_wstring(cp->toString());
    } else if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        text = argIn[0].asWideString();
    } else {
        Error(L"Nelson:validators:mustBeTextScalar",
            L"%s must be a character vector or string scalar.", L"Input");
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(vectorizeText(text));
    return retval;
}
//=============================================================================
