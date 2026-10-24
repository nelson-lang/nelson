//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "notebook_editBuiltin.hpp"
#include "WebDesktopBridge.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __notebook_edit__('file.nlx')
// Ask the web desktop to open a Notebook document in the Notebook editor
// panel. Returns true when the web desktop handled it (its opener is
// registered), false otherwise so the `edit` macro can fall back.
ArrayOfVector
Nelson::NotebookGateway::notebook_editBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring filename = argIn[0].asWideString();
    const bool opened = WebDesktopBridge::getInstance()->openWebNotebook(filename);
    retval << ArrayOf::logicalScalar(opened);
    return retval;
}
//=============================================================================
