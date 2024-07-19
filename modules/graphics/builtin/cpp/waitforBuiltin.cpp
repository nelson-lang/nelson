//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
        Error(_W("Expected graphics object(s)."));
    }

    if (!argIn[0].isGraphicsObject()) {
        Error(_W("Expected graphics object(s)."));
    }
    nelson_handle go = argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* fp = getGraphicsObjectFromGraphicHandle(go);
    if (!fp) {
        Error(_W("Invalid handle."));
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
            Error(_W("Invalid property."), L"Nelson:waitfor:BadProperty");
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
            Error(_W("Invalid property."), L"Nelson:waitfor:BadProperty");
        }
        ArrayOf expectedValue = argIn[2];
        bool isEqual = false;
        bool firstTime = true;

        Context* context = eval->getContext();
        FunctionDef* funcDef = nullptr;
        std::string IsApproxName = "isequaln";
        if (!context->lookupFunction(IsApproxName, funcDef)) {
            Error(_W("isequaln not found."), L"Nelson:waitfor:isequaln");
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
                    Error(_W("isequaln lhs error."), L"Nelson:waitfor:isequaln");
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
