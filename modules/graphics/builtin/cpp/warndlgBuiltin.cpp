//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "warndlgBuiltin.hpp"
#include "MessageBoxBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
warndlgBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 3);
    std::wstring message = argIn.empty()
        ? Nelson::resolveMessageTemplate(
              L"Nelson:graphics:defaultWarning", L"This is the default warning.")
        : argIn[0].asWideString();
    std::wstring title = argIn.size() < 2 ? _W("Warning Dialog") : argIn[1].asWideString();
    std::wstring mode = argIn.size() < 3 ? L"non-modal" : argIn[2].asWideString();
    retval << MessageBox(message, title, L"warn", mode);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
