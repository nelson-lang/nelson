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
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <sys/resource.h>
#endif
#include "RecursionStack.hpp"
#include "i18n.hpp"
#include <iostream>
//=============================================================================
namespace Nelson {
size_t
setRecursionStacksize(size_t sizerstack)
{
    size_t returnedSize = 0;
#ifndef _MSC_VER
    if (getRecursionStacksize() != sizerstack) {
        struct rlimit rl;
        getrlimit(RLIMIT_STACK, &rl);
        rl.rlim_cur = (int)sizerstack;
        if (setrlimit(RLIMIT_STACK, &rl) == 0) {
            return sizerstack;
        }
        returnedSize = getRecursionStacksize();
    }
#else
#endif
    return returnedSize;
}
//=============================================================================
#ifdef _MSC_VER
typedef IMAGE_NT_HEADERS*(__stdcall* RtlImageNtHeaderProc)(void* ImageBase);
#endif
size_t
getRecursionStacksize()
{
    size_t returnedSize = 0;
#ifndef _MSC_VER
    struct rlimit rl;
    int result = getrlimit(RLIMIT_STACK, &rl);
    if (result == 0) {
        returnedSize = (size_t)(rl.rlim_cur);
    }
#else
    HANDLE hProcess = GetCurrentProcess();
    wchar_t buffer[MAX_PATH];
    HMODULE IB = GetModuleHandleW(nullptr);
    GetModuleFileNameW(IB, buffer, MAX_PATH);
    DWORD old = 0;
    HMODULE hModule = LoadLibrary(L"ntdll.dll");
    RtlImageNtHeaderProc RtlImageNtHeaderProcPointer
        = reinterpret_cast<RtlImageNtHeaderProc>(GetProcAddress(hModule, "RtlImageNtHeader"));
    if (RtlImageNtHeaderProcPointer == nullptr) {
        std::cout << _("Error: could not find the function NtOpenFile in library ntdll.dll.\n");
        exit(1);
    }
    VirtualProtectEx(hProcess, (void*)IB, 0x1000, PAGE_READONLY, &old);
    IMAGE_NT_HEADERS* pNt = RtlImageNtHeaderProcPointer(IB);
    if (pNt) {
        returnedSize = static_cast<size_t>(pNt->OptionalHeader.SizeOfStackReserve);
    }
    FreeLibrary(hModule);
#endif
    return returnedSize;
}
//=============================================================================
} // namespace Nelson
