//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UIControlBuilder.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
#include "GOUIControl.h"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOCallbackProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
go_handle
UIControlBuilder(const ArrayOfVector& argIn)
{
    ArrayOfVector copyArgIn(argIn);
    int64 currentFigureID = -1;
    if (copyArgIn.size() > 0) {
        if (copyArgIn[0].isGraphicsObject()) {
            go_handle go = copyArgIn[0].getContentAsGraphicsObjectScalar();
            GOFigure* fig = findGOFigure(go);
            if (fig) {
                currentFigureID = go;
                copyArgIn.pop_front();
            } else {
                GraphicsObject* hp = findGraphicsObject(go);
                if (hp->isType(GO_PROPERTY_VALUE_UICONTROL_STR)) {
                    if (copyArgIn.size() == 1) {
                        GOUIControl* goUIControl = (GOUIControl*)(hp);
                        goUIControl->setFocus();
                    } else {
                        raiseError(L"Nelson:graphics:ERROR_UICONTROL_GRAPHIC_OBJECT_EXPECTED",
                            ERROR_UICONTROL_GRAPHIC_OBJECT_EXPECTED);
                    }
                } else {
                    raiseError(L"Nelson:graphics:ERROR_FIGURE_OR_UICONTROL_GRAPHIC_OBJECT_EXPECTED",
                        ERROR_FIGURE_OR_UICONTROL_GRAPHIC_OBJECT_EXPECTED);
                }
            }
        }
    }
    for (size_t k = 0; k < copyArgIn.size(); k = k + 2) {
        std::wstring propname(copyArgIn[k].getContentAsWideString());
        if (propname == GO_PARENT_PROPERTY_NAME_STR && (k + 1 < copyArgIn.size())) {
            ArrayOf propvalue(copyArgIn[k + 1]);
            go_handle go = propvalue.getContentAsGraphicsObjectScalar();
            GOFigure* fig = findGOFigure(go);
            if (fig) {
                currentFigureID = go;
            } else {
                raiseError(L"Nelson:graphics:ERROR_FIGURE_EXPECTED", ERROR_FIGURE_EXPECTED);
            }
        }
    }

    if (currentFigureID == -1) {
        currentFigureID = getCurrentFigure();
    }
    if (currentFigureID == -1) {
        currentFigureID = createNewFigure();
    }

    GOUIControl* go = new GOUIControl;

    go_handle thisHandle = assignGraphicsObject(go);
    go->buildWidget(findGOWindows(currentFigureID));

    GOGObjectsProperty* cp
        = static_cast<GOGObjectsProperty*>(go->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> parent;
    parent.push_back(currentFigureID);
    cp->data(parent);

    GOGObjectsProperty* hp = (GOGObjectsProperty*)findGOWindows(currentFigureID)
                                 ->getGOFigure()
                                 ->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(hp->data());
    children.push_back(thisHandle);
    hp->data(children);

    GraphicsObject* fp = (GraphicsObject*)go;
    while (copyArgIn.size() >= 2) {
        std::wstring propname(copyArgIn[0].getContentAsWideString());
        ArrayOf propvalue(copyArgIn[1]);
        try {
            GOGenericProperty* goproperty = fp->findProperty(propname);
            if (!fp->isWritable(propname)) {
                raiseError(L"Nelson:graphics:ERROR_PROPERTY_IS_READABLE_ONLY",
                    ERROR_PROPERTY_IS_READABLE_ONLY, propname);
            }
            goproperty->set(propvalue);
        } catch (const Exception& e) {
            std::wstring msg = ERROR_GOT_ERROR_FOR_PROPERTY + L" " + propname + L"\n" + e.what();
            Error(msg, L"Nelson:graphics:ERROR_GOT_ERROR_FOR_PROPERTY");
        }
        copyArgIn.pop_front();
        copyArgIn.pop_front();
    }
    fp->updateState();

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)fp->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->executeNow(fp);
    }

    return thisHandle;
}
//=============================================================================
}
//=============================================================================
