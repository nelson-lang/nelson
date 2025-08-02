//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GOArrayOfProperty.hpp"
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
#include "GraphicCallback.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOCallbackProperty : public GOArrayOfProperty
{
private:
    GraphicCallback
    buildGraphicCallback(GraphicsObject* go, const std::wstring& className,
        const std::wstring& EventName, const std::wstring& character, const std::wstring& key,
        const wstringVector& modifier);
    GraphicCallback
    buildGraphicCallback(
        GraphicsObject* go, const std::wstring& className, const std::wstring& actionName);
    GraphicCallback
    buildGraphicCallback(GraphicsObject* go);

public:
    GOCallbackProperty() : GOArrayOfProperty() { }
    ~GOCallbackProperty() override = default;
    void set(ArrayOf) override;
    ArrayOf
    get() override;
    std::wstring
    toWideString() override;

    void
    pushKeyEvent(GraphicsObject* go, const std::wstring& className, const std::wstring& EventName,
        const std::wstring& character, const std::wstring& key, const wstringVector& modifier);
    void
    pushEvent(GraphicsObject* go, const std::wstring& className, const std::wstring& actionName);
    void
    pushEvent(GraphicsObject* go);
    void
    executeNow(GraphicsObject* go);
    void
    executeNow(GraphicsObject* go, const std::wstring& className, const std::wstring& actionName);
};
//=============================================================================
};
//=============================================================================
