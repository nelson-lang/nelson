//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Windows.h>
#include "ComEngine.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ComEngine* ComEngine::m_pInstance = nullptr;
//=============================================================================
ComEngine*
ComEngine::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new ComEngine();
    }
    return m_pInstance;
}
//=============================================================================
ComEngine::ComEngine() { isInitialized = false; }
//=============================================================================
void
ComEngine::create()
{
    if (!isInitialized) {
        ::CoInitialize(nullptr);
        isInitialized = true;
    }
}
//=============================================================================
void
ComEngine::finish()
{
    ::CoUninitialize();
    isInitialized = false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
