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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GraphicsEnvironment.hpp"
#include "QueueProcessing.hpp"
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

#if defined(__EMSCRIPTEN__)
    // The single-threaded WebAssembly worker cannot busy-wait for a GUI event:
    // spinning here blocks the worker, so the awaited close/property/press event
    // could never be delivered. Fail fast instead of hanging the browser.
    Error(L"Nelson:waitfor:unsupportedInBrowser",
        _W("waitfor is not available in the WebAssembly runtime."));
#endif

    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }

    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }
    nelson_handle go = argIn[0].asGraphicsObjectScalar();
    GraphicsObject* fp = getGraphicsObjectFromGraphicHandle(go);
    if (!fp) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }

    switch (argIn.size()) {
    case 1: {
        // waitfor(h)
        do {
            GraphicsEnvironment::processGuiEvents();
            processPendingCallbacksAndTimers(eval);
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            fp = getGraphicsObjectFromGraphicHandle(go);
        } while (fp != nullptr);
    } break;
    case 2: {
        // waitfor(h, propertyName)
        std::wstring propertyName = argIn[1].asWideString();
        GOGenericProperty* property = fp->findProperty(propertyName, false);
        if (property == nullptr) {
            Error(L"Nelson:waitfor:BadProperty", L"Invalid property.");
        }
        do {
            GraphicsEnvironment::processGuiEvents();
            processPendingCallbacksAndTimers(eval);
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            fp = getGraphicsObjectFromGraphicHandle(go);
        } while (fp != nullptr && !property->isModified());
    } break;
    case 3: {
        // waitfor(h, propertyName, value)
        std::wstring propertyName = argIn[1].asWideString();
        GOGenericProperty* goProperty = fp->findProperty(propertyName, false);
        if (goProperty == nullptr) {
            Error(L"Nelson:waitfor:BadProperty", L"Invalid property.");
        }
        ArrayOf expectedValue = argIn[2];
        bool isEqual = false;
        bool firstTime = true;

        Context* context = eval->getContext();
        FunctionDef* funcDef = nullptr;
        std::string IsApproxName = "isequaln";
        if (!context->lookupFunction(IsApproxName, funcDef)) {
            Error(L"Nelson:waitfor:isequalnNotFound", L"isequaln not found.");
        }
        do {
            GraphicsEnvironment::processGuiEvents();
            processPendingCallbacksAndTimers(eval);
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            fp = getGraphicsObjectFromGraphicHandle(go);
            if (fp == nullptr) {
                break;
            }
            if ((goProperty->isModified() || firstTime)) {
                firstTime = false;
                ArrayOf currentValue = goProperty->get();
                ArrayOfVector inputs;
                inputs << currentValue;
                inputs << expectedValue;
                ArrayOfVector IsEqualResult = funcDef->evaluateFunction(eval, inputs, 1);
                if (IsEqualResult.size() != 1) {
                    Error(L"Nelson:waitfor:isequalnLhsError", L"isequaln lhs error.");
                }
                isEqual = IsEqualResult[0].asLogicalScalar();
            }
        } while (fp != nullptr && !isEqual);

    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
