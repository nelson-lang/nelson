//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
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
        } else {
            returnedSize = getRecursionStacksize();
        }
    }
#else
#endif
    return returnedSize;
}
//=============================================================================
#ifdef _MSC_VER
typedef IMAGE_NT_HEADERS*(__stdcall* RtlImageNtHeaderProc)(void* ImageBase);
static RtlImageNtHeaderProc RtlImageNtHeaderProcPointer;
static bool bFirstCall = true;
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
    HMODULE IB = GetModuleHandleW(NULL);
    GetModuleFileNameW(IB, buffer, MAX_PATH);
    DWORD old = 0;
    if (bFirstCall) {
        HMODULE hModule = LoadLibrary(L"ntdll.dll");
        RtlImageNtHeaderProcPointer
            = (RtlImageNtHeaderProc)GetProcAddress(hModule, "RtlImageNtHeader");
        if (RtlImageNtHeaderProcPointer == nullptr) {
            std::cout << _("Error: could not find the function NtOpenFile in library ntdll.dll.\n");
            exit(1);
        }
        bFirstCall = false;
    }
    VirtualProtectEx(hProcess, (void*)IB, 0x1000, PAGE_READONLY, &old);
    IMAGE_NT_HEADERS* pNt = RtlImageNtHeaderProcPointer(IB);
    if (pNt) {
        returnedSize = (size_t)(pNt->OptionalHeader.SizeOfStackReserve);
    }
#endif
    return returnedSize;
}
//=============================================================================
}
