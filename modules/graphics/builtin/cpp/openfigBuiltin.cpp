//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "openfigBuiltin.hpp"
#include "FigurePersistence.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
openfigBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf payload = argIn[0];
    std::wstring filename = argIn[1].asWideString();
    std::wstring visibleAction;
    bool reuse = false;
    if (argIn.size() >= 3) {
        visibleAction = argIn[2].asWideString();
    }
    if (argIn.size() >= 4) {
        reuse = argIn[3].asLogicalScalar();
    }

    ArrayOfVector retval;
    retval << restoreFiguresFromFigPayload(payload, filename, visibleAction, reuse);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
