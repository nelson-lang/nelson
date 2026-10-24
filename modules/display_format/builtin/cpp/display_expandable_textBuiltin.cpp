//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "display_expandable_textBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Interface.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DisplayFormatGateway::display_expandable_textBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    std::wstring label = argIn[0].asWideString();
    std::wstring expandedText = argIn[1].asWideString();
    std::wstring plainFallback;
    if (argIn.size() == 3) {
        plainFallback = argIn[2].asWideString();
    }
    bool handled = false;
    if (eval && eval->getInterface()) {
        handled = eval->getInterface()->outputExpandableText(label, expandedText, plainFallback);
    }
    if (nLhs > 0) {
        retval << ArrayOf::logicalScalar(handled);
    }
    return retval;
}
//=============================================================================
