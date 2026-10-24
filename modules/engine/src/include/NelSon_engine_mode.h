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
    GUI = 4,
    WEB_GUI = 5,
    // Terminal hosted by a WebAssembly worker: same evaluator as BASIC_TERMINAL,
    // the browser desktop connects through PortableInterface/WebDesktopBridge.
    WASM_TERMINAL = 6
};
//=============================================================================
