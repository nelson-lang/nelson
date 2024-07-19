//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "figureBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RefreshFigure.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "CallbackQueue.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
figureBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval = {};

    Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    CallbackQueue::getInstance()->processCallback(eval);

    int64 currentFigureID;
    if (argIn.size() == 0) {
        currentFigureID = createNewFigure();
        retval << ArrayOf::graphicsObjectConstructor(currentFigureID);
        return retval;
    }
    size_t pos = 0;
    if (argIn[0].isNumeric()) {
        int64 fignum = argIn[0].getContentAsInteger64Scalar();
        if ((fignum <= 0) || (fignum > MAX_FIGS)) {
            Error(_("figure number is out of range - it must be between 1 and 2147483647."));
        }
        currentFigureID = selectFigure(fignum - 1, false);
        pos = pos + 1;
    } else if (argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
        if (handle == HANDLE_ROOT_OBJECT || handle >= HANDLE_OFFSET_OBJECT) {
            Error(_("figure graphics object expected."));
        } else {
            currentFigureID = selectFigure(handle, false);
            pos = pos + 1;
        }
    } else {
        currentFigureID = createNewFigure(false);
    }
    GOFigure* fig = findGOFigure(currentFigureID);
    if (!fig) {
        Error(_W("Invalid Figure handle."));
    }
    if ((argIn.size() - pos) % 2 == 0) {
        GraphicsObject* go = static_cast<GraphicsObject*>(fig);
        for (size_t k = pos; k < argIn.size(); k = k + 2) {
            if ((argIn[k].isStringArray() && argIn[k].isScalar())
                || argIn[k].isRowVectorCharacterArray()) {
                const std::wstring propertyName = argIn[k].getContentAsWideString();
                if (!go->isWritable(propertyName)) {
                    Error(_W("Property is readable only: ") + propertyName);
                }
                GOGenericProperty* propertyValue = go->findProperty(propertyName);
                propertyValue->set(argIn[k + 1]);
            }
        }
        fig->updateState(true);
    } else {
        Error(_("Wrong number of input parameters."));
    }
    retval << ArrayOf::graphicsObjectConstructor(currentFigureID);

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)fig->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->executeNow(fig);
    }

    return retval;
}
//=============================================================================
}
//=============================================================================
