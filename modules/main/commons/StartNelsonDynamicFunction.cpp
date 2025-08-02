//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <string>
#include <intrin.h>
#include <windows.h>
#include "StartNelsonDynamicFunction.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static HMODULE nlsEngineHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static bool
IsWindows10OrGreater()
{
    typedef NTSTATUS(WINAPI * RtlGetVersionPtr)(PRTL_OSVERSIONINFOW);
    RTL_OSVERSIONINFOW rovi = { 0 };
    rovi.dwOSVersionInfoSize = sizeof(rovi);
#define MINIMAL_WINDOWS_VERSION 10
    HMODULE hMod = GetModuleHandleW(L"ntdll.dll");
    if (hMod) {
        RtlGetVersionPtr pRtlGetVersion = (RtlGetVersionPtr)GetProcAddress(hMod, "RtlGetVersion");
        if (pRtlGetVersion) {
            pRtlGetVersion(&rovi);
            return (rovi.dwMajorVersion == MINIMAL_WINDOWS_VERSION);
        }
    }
    return false;
}
//=============================================================================
static bool
isAVX2Supported()
{
    int cpuInfo[4];

    // First check if CPUID supports extended features (EAX=7)
    __cpuid(cpuInfo, 0);
    if (cpuInfo[0] < 7) {
        return false; // CPU doesn't support CPUID leaf 7
    }

    // Call CPUID with EAX=7, ECX=0 to get extended features
    __cpuidex(cpuInfo, 7, 0);

    // AVX2 is indicated by bit 5 of EBX
    return (cpuInfo[1] & (1 << 5)) != 0;
}
//=============================================================================
static bool
initEngineDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathEngineSharedLibrary = L"./libnlsEngine.dll";
        nlsEngineHandleDynamicLibrary = LoadLibraryW(fullpathEngineSharedLibrary.c_str());
        if (nlsEngineHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
            return true;
        }
    }
    return nlsEngineHandleDynamicLibrary != nullptr;
}
//=============================================================================
int
StartNelsonDynamicFunction(int argc, wchar_t* argv[], NELSON_ENGINE_MODE _mode)
{
    if (!IsWindows10OrGreater()) {
        if (_mode == NELSON_ENGINE_MODE::GUI) {
            MessageBoxA(NULL, "Nelson requires at least Windows 10.", "ERROR", MB_OK);
        } else {
            fprintf(stderr, "\nNelson requires at least Windows 10.\n");
        }
        return -1;
    }
#ifdef _WIN64
    if (!isAVX2Supported()) {
        if (_mode == NELSON_ENGINE_MODE::GUI) {
            MessageBoxA(
                NULL, "Nelson needs a CPU that supports the AVX2 instruction set.", "ERROR", MB_OK);
        } else {
            fprintf(stderr, "\nNelson needs a CPU that supports the AVX2 instruction set.\n");
        }
        return -1;
    }
#endif
    using PROC_StartNelson = int (*)(int argc, wchar_t* argv[], NELSON_ENGINE_MODE _mode);

    if (!initEngineDynamicLibrary()) {
        if (_mode == NELSON_ENGINE_MODE::GUI) {
            MessageBoxA(NULL, "Impossible to find 'libnlsengine.dll'.", "ERROR", MB_OK);
        } else {
            fprintf(stderr, "\nImpossible to find 'libnlsengine.dll'.\n");
        }
        return -1;
    }

    PROC_StartNelson StartNelsonPtr = reinterpret_cast<PROC_StartNelson>(
        GetProcAddress(nlsEngineHandleDynamicLibrary, "StartNelsonW"));
    int exitCode = -1;
    if (!StartNelsonPtr) {
        if (_mode == NELSON_ENGINE_MODE::GUI) {
            MessageBoxA(NULL, "Impossible to find symbol 'StartNelsonW'.", "ERROR", MB_OK);
        } else {
            fprintf(stderr, "\nImpossible to find symbol 'StartNelsonW'.\n");
        }
        return exitCode;
    }
    exitCode = StartNelsonPtr(argc, argv, _mode);
    FreeLibrary(nlsEngineHandleDynamicLibrary);
    nlsEngineHandleDynamicLibrary = nullptr;
    bFirstDynamicLibraryCall = true;
    return exitCode;
}
//=============================================================================
}
//=============================================================================
#endif /* _MSC_VER windows only */
//=============================================================================
