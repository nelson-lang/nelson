//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "helpdlgBuiltin.hpp"
#include "MessageBoxBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
helpdlgBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 2);
    std::wstring message
        = argIn.empty() ? _W("This is the default help.") : argIn[0].asWideString();
    std::wstring title = argIn.size() < 2 ? _W("Help Dialog") : argIn[1].asWideString();
    retval << MessageBox(message, title, L"help", L"non-modal");
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
