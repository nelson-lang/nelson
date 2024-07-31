//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOCallbackProperty : public GOArrayOfProperty
{
public:
    GOCallbackProperty() : GOArrayOfProperty() { }
    ~GOCallbackProperty() override = default;
    void set(ArrayOf) override;
    ArrayOf
    get() override;
    std::wstring
    toWideString() override;

    bool
    pushKeyEvent(GraphicsObject* go, const std::wstring& className, const std::wstring& EventName,
        const std::wstring& character, const std::wstring& key, wstringVector modifier);
    bool
    pushEvent(GraphicsObject* go, const std::wstring& className, const std::wstring& actionName);
    bool
    pushEvent(GraphicsObject* go);
    bool
    executeNow(GraphicsObject* go);
    bool
    executeNow(GraphicsObject* go, const std::wstring& className, const std::wstring& actionName);
};
//=============================================================================
};
//=============================================================================
