//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <dlfcn.h>
#endif
//=============================================================================
// Qt does not support unloading Qt Quick: it leaves process-wide callbacks registered
// (accessibility factories, ...) until the QApplication is destroyed. This library is the
// only importer of Qt6Qml/Qt6Quick, so removemodule('qml_engine') would unmap them and the
// next accessibility query or teardown would jump into unmapped pages.
// Pinning the library on first load keeps the dependency chain mapped for the process
// lifetime. Removing the module still unregisters its builtins.
//=============================================================================
namespace {
//=============================================================================
void
pinAnchorSymbol()
{
}
//=============================================================================
struct QmlEngineModulePin
{
    QmlEngineModulePin()
    {
#ifdef _WIN32
        HMODULE moduleHandle = nullptr;
        GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_PIN | GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
            reinterpret_cast<LPCWSTR>(reinterpret_cast<void*>(&pinAnchorSymbol)), &moduleHandle);
#else
        Dl_info info;
        if (dladdr(reinterpret_cast<void*>(&pinAnchorSymbol), &info) != 0
            && info.dli_fname != nullptr) {
            if (dlopen(info.dli_fname, RTLD_NOW | RTLD_NOLOAD | RTLD_NODELETE) == nullptr) {
                // RTLD_NODELETE promotion unavailable: keep an extra reference forever.
                dlopen(info.dli_fname, RTLD_NOW);
            }
        }
#endif
    }
};
//=============================================================================
QmlEngineModulePin qmlEngineModulePin;
//=============================================================================
} // namespace
//=============================================================================
