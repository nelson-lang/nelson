//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOConstructorHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "GOFiguresManager.hpp"
#include "axesBuiltin.hpp"
#include "GOAxis.hpp"
#include "GOGObjectsProperty.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
go_handle
GOCommonConstructorHelper(GraphicsObject* fp, const ArrayOfVector& arg)
{
    bool isAutoParent = true;
    go_handle handle = assignGraphicsObject(fp);
    ArrayOfVector t(arg);
    while (t.size() >= 2) {
        std::wstring propname(t[0].getContentAsWideString());
        if (propname == GO_AUTO_PARENT_PROPERTY_NAME_STR) {
            isAutoParent = (t[1].getContentAsWideString() == GO_PROPERTY_VALUE_ON_STR);
        } else {
            try {
                fp->findProperty(propname)->set(t[1]);
            } catch (const Exception& e) {
                Error(_W("Got error for property:") + L" " + propname + L"\n" + e.what());
            }
        }
        t.pop_front();
        t.pop_front();
    }

    if (isAutoParent) {
        GOFigure* fig = getCurrentGOFigure();
        int64 current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
        if (current == 0) {
            ArrayOfVector arg2;
            axesBuiltin(0, arg2);
            current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
        }
        GOAxis* axis = (GOAxis*)findGraphicsObject(current);
        GOGObjectsProperty* cp
            = (GOGObjectsProperty*)axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
        std::vector<int64> children(cp->data());
        children.push_back(handle);
        cp->data(children);
        cp = (GOGObjectsProperty*)fp->findProperty(GO_PARENT_PROPERTY_NAME_STR);
        std::vector<int64> parent;
        parent.push_back(current);
        cp->data(parent);
        axis->updateState();
    }
    fp->updateState();
    return handle;
}
//=============================================================================
}
//=============================================================================
