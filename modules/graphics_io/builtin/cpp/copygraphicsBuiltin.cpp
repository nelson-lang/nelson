//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include <QtGui/QClipboard>
#include "copygraphicsBuiltin.hpp"
#include "GOWindow.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsIoGateway {
//=============================================================================
ArrayOfVector
copygraphicsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval = {};
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
    GOWindow* f = findGOWindows(handle);
    if (!f) {
        Error(_W("Invalid handle."));
    }
    f->getGOFigure()->setRenderingStateInvalid(true);
    QClipboard* cb = QApplication::clipboard();
    cb->setPixmap(f->getMainQWigdet()->grab());
    return retval;
}
//=============================================================================
}
//=============================================================================
