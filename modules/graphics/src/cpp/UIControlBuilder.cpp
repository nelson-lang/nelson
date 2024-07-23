//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "i18n.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOCallbackProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
go_handle
UIControlBuilder(const ArrayOfVector& argIn)
{
    Evaluator* mainEvaluator = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();

    int64 currentFigureID = -1;
    for (size_t k = 0; k < argIn.size(); k = k + 2) {
        std::wstring propname(argIn[k].getContentAsWideString());
        if (propname == GO_PARENT_PROPERTY_NAME_STR && (k + 1 < argIn.size())) {
            ArrayOf propvalue(argIn[k + 1]);
            go_handle hparent = propvalue.getContentAsGraphicsObjectScalar();
            GraphicsObject* hp = findGraphicsObject(hparent);
            if (!hp->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
                Error(_W("Figure expected."));
            } else {
                GOScalarProperty* fp
                    = (GOScalarProperty*)hp->findProperty(GO_NUMBER_PROPERTY_NAME_STR);
                currentFigureID = (int)fp->data();
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
    go->setEvaluator(mainEvaluator);

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
    ArrayOfVector copyArgIn(argIn);
    while (copyArgIn.size() >= 2) {
        std::wstring propname(copyArgIn[0].getContentAsWideString());
        ArrayOf propvalue(copyArgIn[1]);
        try {
            fp->findProperty(propname)->set(propvalue);
        } catch (const Exception& e) {
            Error(_W("Got error for property:") + L" " + propname + L"\n" + e.what());
        }
        copyArgIn.pop_front();
        copyArgIn.pop_front();
    }

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
