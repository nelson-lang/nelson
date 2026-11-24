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
enum NELSON_ENGINE_MODE
{
    BASIC_ENGINE = 0,
    ADVANCED_ENGINE = 1,
    BASIC_TERMINAL = 2,
    ADVANCED_TERMINAL = 3,
    // Desktop: the Qt-free desktop (gui frontend + webview), loaded
    // dynamically at runtime so terminal binaries never link the web stack.
    GUI = 4,
    // Terminal hosted by a WebAssembly worker: same evaluator as BASIC_TERMINAL,
    // the browser desktop connects through PortableInterface/WebDesktopBridge.
    // Value 5 (the former WEB_GUI mode) was folded into GUI and retired.
    WASM_TERMINAL = 6
};
//=============================================================================
