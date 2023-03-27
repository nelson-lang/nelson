//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RefreshFigure.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GraphicsObject.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
refreshFigure(go_handle go)
{
    GOFigure* fig = findGOFigure(go);
    if (fig) {
        try {
            fig->updateState();
            GOGObjectsProperty* children
                = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
            std::vector<int64> handles(children->data());
            for (int i = 0; i < handles.size(); i++) {
                GraphicsObject* fp = findGraphicsObject(handles[i]);
                fp->updateState();
            }
            fig->repaint();
        } catch (const Exception&) {
            return;
        }
    }
}
//=============================================================================
}
//=============================================================================
