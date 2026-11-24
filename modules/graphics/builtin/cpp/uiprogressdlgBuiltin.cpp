//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uiprogressdlgBuiltin.hpp"
#include "GOProgressDialog.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GOGenericProperty.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::uiprogressdlgBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    ArrayOfVector args(argIn);
    // The first argument is the parent figure handle; it is accepted for call
    // compatibility. The dialog is served through the provider, not parented
    // into the figure canvas.
    if (!args.empty() && args[0].isGraphicsObject()) {
        args.pop_front();
    }
    auto* go = new GOProgressDialog();
    go_handle handle = assignGraphicsObject(go);
    for (size_t k = 0; k + 1 < args.size(); k += 2) {
        const std::wstring name = go->resolvePartialPropertyName(args[k].asWideString());
        go->findProperty(name)->set(args[k + 1]);
    }
    go->updateState();
    ArrayOfVector retval;
    retval << graphicsObjectHandle(handle);
    return retval;
}
//=============================================================================
