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
#include "refreshBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GraphicsObject.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"

//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
refreshBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOfVector retval = {};
    int64 handle;
    GOFigure* fig = nullptr;
    if (argIn.size() == 0) {
        handle = getCurrentFigure();
        if (handle == -1) {
            handle = createNewFigure();
        }
    } else {
        if (argIn[0].isGraphicsObject() && argIn[0].isScalar()) {
            handle = argIn[0].getContentAsGraphicsObjectScalar();
        } else {
            Error(_("figure graphics object expected."));
        }
    }
    if (handle == HANDLE_ROOT_OBJECT || handle >= HANDLE_OFFSET_OBJECT) {
        Error(_("figure graphics object expected."));
    }
    fig = findGOFigure(handle);
    if (fig) {
        fig->updateState();
        GOGObjectsProperty* children
            = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
        std::vector<int64> handles(children->data());
        for (int i = 0; i < handles.size(); i++) {
            GraphicsObject* fp = findGraphicsObject(handles[i]);
            fp->updateState();
        }
        fig->repaint();
    }
    return retval;
}
//=============================================================================
}
//=============================================================================
