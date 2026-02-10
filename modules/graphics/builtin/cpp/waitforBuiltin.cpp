//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <ctime>
#include <thread>
#include <chrono>
#include "waitforBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOFiguresManager.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "ProcessEvents.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static GraphicsObject*
getGraphicsObjectFromGraphicHandle(nelson_handle go)
{
    GraphicsObject* fp = nullptr;
    if (go == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
    } else if (go >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(go, false);
    } else {
        fp = static_cast<GraphicsObject*>(findGOFigure(go));
    }
    return fp;
}
//=============================================================================
ArrayOfVector
GraphicsGateway::waitforBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 0);

    if (!argIn[0].isGraphicsObject()) {
        raiseError(
            L"Nelson:graphics:ERROR_EXPECTED_GRAPHICS_OBJECT_S", ERROR_EXPECTED_GRAPHICS_OBJECT_S);
    }

    if (!argIn[0].isGraphicsObject()) {
        raiseError(
            L"Nelson:graphics:ERROR_EXPECTED_GRAPHICS_OBJECT_S", ERROR_EXPECTED_GRAPHICS_OBJECT_S);
    }
    nelson_handle go = argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* fp = getGraphicsObjectFromGraphicHandle(go);
    if (!fp) {
        raiseError(L"Nelson:graphics:ERROR_INVALID_NELSON_HANDLE", ERROR_INVALID_NELSON_HANDLE);
    }

    switch (argIn.size()) {
    case 1: {
        // waitfor(h)
        do {
            ProcessEvents();
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            fp = getGraphicsObjectFromGraphicHandle(go);
        } while (fp != nullptr);
    } break;
    case 2: {
        // waitfor(h, propertyName)
        std::wstring propertyName = argIn[1].getContentAsWideString();
        GOGenericProperty* property = fp->findProperty(propertyName, false);
        if (property == nullptr) {
            raiseError(L"Nelson:waitfor:ERROR_INVALID_PROPERTY", ERROR_INVALID_PROPERTY);
        }
        do {
            ProcessEvents();
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            fp = getGraphicsObjectFromGraphicHandle(go);
        } while (fp != nullptr && !property->isModified());
    } break;
    case 3: {
        // waitfor(h, propertyName, value)
        std::wstring propertyName = argIn[1].getContentAsWideString();
        GOGenericProperty* goProperty = fp->findProperty(propertyName, false);
        if (goProperty == nullptr) {
            raiseError(L"Nelson:waitfor:ERROR_INVALID_PROPERTY", ERROR_INVALID_PROPERTY);
        }
        ArrayOf expectedValue = argIn[2];
        bool isEqual = false;
        bool firstTime = true;

        Context* context = eval->getContext();
        FunctionDef* funcDef = nullptr;
        std::string IsApproxName = "isequaln";
        if (!context->lookupFunction(IsApproxName, funcDef)) {
            raiseError(L"Nelson:waitfor:ERROR_ISEQUALN_NOT_FOUND", ERROR_ISEQUALN_NOT_FOUND);
        }
        do {
            ProcessEvents();
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            fp = getGraphicsObjectFromGraphicHandle(go);
            if ((goProperty->isModified() || firstTime)) {
                firstTime = false;
                ArrayOf currentValue = goProperty->get();
                ArrayOfVector inputs;
                inputs << currentValue;
                inputs << expectedValue;
                ArrayOfVector IsEqualResult = funcDef->evaluateFunction(eval, inputs, 1);
                if (IsEqualResult.size() != 1) {
                    raiseError(
                        L"Nelson:waitfor:ERROR_ISEQUALN_LHS_ERROR", ERROR_ISEQUALN_LHS_ERROR);
                }
                isEqual = IsEqualResult[0].getContentAsLogicalScalar();
            }
        } while (fp != nullptr && !isEqual);

    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
