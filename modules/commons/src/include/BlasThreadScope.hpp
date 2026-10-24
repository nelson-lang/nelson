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
#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef min
#undef max
#endif
#include <cstdint>
//=============================================================================
namespace Nelson {
//=============================================================================
class BlasThreadScope
{
public:
    BlasThreadScope(bool enabled, int requestedThreads = 1)
    {
        if (!enabled || requestedThreads < 1) {
            return;
        }
        ThreadApi api = getThreadApi();
        if (api.getMaxThreads == nullptr || api.setNumThreads == nullptr) {
            return;
        }
        previousThreads = api.getMaxThreads();
        if (previousThreads > requestedThreads) {
            api.setNumThreads(requestedThreads);
            active = true;
        }
    }

    ~BlasThreadScope()
    {
        if (active) {
            ThreadApi api = getThreadApi();
            if (api.setNumThreads != nullptr) {
                api.setNumThreads(previousThreads);
            }
        }
    }

    BlasThreadScope(const BlasThreadScope&) = delete;
    BlasThreadScope&
    operator=(const BlasThreadScope&)
        = delete;

private:
    using GetMaxThreadsPtr = int (*)();
    using SetNumThreadsPtr = void (*)(int);

    struct ThreadApi
    {
        GetMaxThreadsPtr getMaxThreads = nullptr;
        SetNumThreadsPtr setNumThreads = nullptr;
    };

    static ThreadApi
    getThreadApi()
    {
#ifdef _WIN32
        static ThreadApi api = []() {
            ThreadApi resolved;
            HMODULE module = GetModuleHandleW(L"libiomp5md.dll");
            if (module == nullptr) {
                module = LoadLibraryW(L"libiomp5md.dll");
            }
            if (module != nullptr) {
                resolved.getMaxThreads = reinterpret_cast<GetMaxThreadsPtr>(
                    GetProcAddress(module, "omp_get_max_threads"));
                resolved.setNumThreads = reinterpret_cast<SetNumThreadsPtr>(
                    GetProcAddress(module, "omp_set_num_threads"));
            }
            return resolved;
        }();
        return api;
#else
        return {};
#endif
    }

    int previousThreads = 1;
    bool active = false;
};
//=============================================================================
inline bool
useSingleThreadBlasForSmallMatrix(int rows, int columns)
{
    if (rows <= 0 || columns <= 0) {
        return false;
    }
    return static_cast<uint64_t>(rows) * static_cast<uint64_t>(columns) <= 262144ULL;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
