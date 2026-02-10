//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "figureBuiltin.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
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
#include "QueueProcessing.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
figureBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval = {};

    processPendingCallbacksAndTimers();

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
            raiseError(L"Nelson:graphics:ERROR_FIGURE_NUMBER_IS_OUT_OF_RANGE_IT_MUST_BE_BETWEEN_1_"
                       L"AND_2147483647",
                ERROR_FIGURE_NUMBER_IS_OUT_OF_RANGE_IT_MUST_BE_BETWEEN_1_AND_2147483647);
        }
        currentFigureID = selectFigure(fignum - 1, false);
        pos = pos + 1;
    } else if (argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
        if (handle == HANDLE_ROOT_OBJECT || handle >= HANDLE_OFFSET_OBJECT) {
            raiseError(L"Nelson:graphics:ERROR_FIGURE_GRAPHICS_OBJECT_EXPECTED",
                ERROR_FIGURE_GRAPHICS_OBJECT_EXPECTED);
        } else {
            currentFigureID = selectFigure(handle, false);
            pos = pos + 1;
        }
    } else {
        currentFigureID = createNewFigure(false);
    }
    GOFigure* fig = findGOFigure(currentFigureID);
    if (!fig) {
        raiseError(L"Nelson:graphics:ERROR_INVALID_FIGURE_HANDLE", ERROR_INVALID_FIGURE_HANDLE);
    }
    if ((argIn.size() - pos) % 2 == 0) {
        GraphicsObject* go = static_cast<GraphicsObject*>(fig);
        for (size_t k = pos; k < argIn.size(); k = k + 2) {
            if ((argIn[k].isStringArray() && argIn[k].isScalar())
                || argIn[k].isRowVectorCharacterArray()) {
                const std::wstring propertyName = argIn[k].getContentAsWideString();
                if (!go->isWritable(propertyName) && go->haveProperty(propertyName)) {
                    raiseError(L"Nelson:graphics:ERROR_PROPERTY_IS_READABLE_ONLY",
                        ERROR_PROPERTY_IS_READABLE_ONLY, propertyName);
                }
                GOGenericProperty* propertyValue = go->findProperty(propertyName);
                propertyValue->set(argIn[k + 1]);
            }
        }
        fig->updateState(true);
    } else {
        raiseError(L"Nelson:graphics:ERROR_WRONG_NUMBER_OF_INPUT_PARAMETERS",
            ERROR_WRONG_NUMBER_OF_INPUT_PARAMETERS);
    }
    retval << ArrayOf::graphicsObjectConstructor(currentFigureID);

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)fig->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->executeNow(fig);
    }
    fig->setRenderingStateInvalid(true);
    return retval;
}
//=============================================================================
}
//=============================================================================
