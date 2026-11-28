//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FatalErrorReport.hpp"
//=============================================================================
#if defined(_MSC_VER) && defined(_WIN64) && !defined(_DEBUG)
#define NELSON_FATAL_ERROR_REPORT
#include <Windows.h>
#include <atomic>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <intrin.h>
#include <typeinfo>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef NELSON_FATAL_ERROR_REPORT
// Code of a C++ exception raised by the compiler runtime.
static constexpr DWORD CPP_EXCEPTION_CODE = 0xE06D7363;
static constexpr USHORT MAX_STACK_FRAMES = 32;
static constexpr size_t TEXT_SIZE = 512;
// Layout of the data the compiler attaches to a thrown C++ object (64 bit:
// every reference is an offset from the image base).
struct ThrownTypeDescriptor
{
    const void* vftable;
    void* spare;
    char name[1];
};
struct ThrownCatchableType
{
    DWORD properties;
    int typeDescriptor;
    int memberDisplacement;
    int vbtableDisplacement;
    int vbaseDisplacement;
    int sizeOrOffset;
    int copyFunction;
};
struct ThrownCatchableTypeArray
{
    int count;
    int types[1];
};
struct ThrownInfo
{
    DWORD attributes;
    int unwindFunction;
    int forwardCompatibility;
    int catchableTypeArray;
};
//=============================================================================
static std::atomic_flag alreadyReported = ATOMIC_FLAG_INIT;
static LPTOP_LEVEL_EXCEPTION_FILTER previousExceptionFilter = nullptr;
//=============================================================================
static LONG WINAPI
onUnhandledException(EXCEPTION_POINTERS* info);
static void
onAbort(int signalNumber);
static void
onInvalidParameter(const wchar_t* expression, const wchar_t* function, const wchar_t* file,
    unsigned int line, uintptr_t reserved);
static void
onPureCall();
static void
describeCppException(const EXCEPTION_RECORD* record, char* text, size_t textSize);
static void
describeCurrentException(char* text, size_t textSize);
static void
readableTypeName(const char* decorated, char* text, size_t textSize);
static void
reportFatalError(const char* reason, bool withStack);
static void
writeStack();
static const char*
nearestExportedName(HMODULE module, const void* address, size_t* displacement);
static void
writeText(const char* text);
#endif
//=============================================================================
void
installFatalErrorReport()
{
#ifdef NELSON_FATAL_ERROR_REPORT
    static std::atomic_bool installed(false);
    if (installed.exchange(true)) {
        return;
    }
    previousExceptionFilter = SetUnhandledExceptionFilter(onUnhandledException);
    std::signal(SIGABRT, onAbort);
    _set_invalid_parameter_handler(onInvalidParameter);
    _set_purecall_handler(onPureCall);
#endif
}
//=============================================================================
#ifdef NELSON_FATAL_ERROR_REPORT
static LONG WINAPI
onUnhandledException(EXCEPTION_POINTERS* info)
{
    char reason[TEXT_SIZE] = "unhandled exception";
    bool withStack = true;
    if (info != nullptr && info->ExceptionRecord != nullptr) {
        const EXCEPTION_RECORD* record = info->ExceptionRecord;
        if (record->ExceptionCode == CPP_EXCEPTION_CODE) {
            describeCppException(record, reason, sizeof(reason));
        } else if (record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION
            && record->NumberParameters >= 2) {
            std::snprintf(reason, sizeof(reason), "access violation %s address 0x%llx",
                record->ExceptionInformation[0] == 0 ? "reading" : "writing",
                static_cast<unsigned long long>(record->ExceptionInformation[1]));
        } else {
            std::snprintf(reason, sizeof(reason), "system exception 0x%08lx",
                static_cast<unsigned long>(record->ExceptionCode));
        }
        // Nothing is left to walk the frames with once the stack is exhausted.
        withStack = record->ExceptionCode != EXCEPTION_STACK_OVERFLOW;
    }
    reportFatalError(reason, withStack);
    if (previousExceptionFilter != nullptr) {
        return previousExceptionFilter(info);
    }
    return EXCEPTION_CONTINUE_SEARCH;
}
//=============================================================================
static void
onAbort(int signalNumber)
{
    (void)signalNumber;
    char reason[TEXT_SIZE];
    describeCurrentException(reason, sizeof(reason));
    reportFatalError(reason, true);
}
//=============================================================================
static void
onInvalidParameter(const wchar_t* expression, const wchar_t* function, const wchar_t* file,
    unsigned int line, uintptr_t reserved)
{
    (void)expression;
    (void)function;
    (void)file;
    (void)line;
    (void)reserved;
    reportFatalError("invalid parameter passed to a C runtime function", true);
    // Same end as without this handler: the process stops here.
    __fastfail(FAST_FAIL_INVALID_ARG);
}
//=============================================================================
static void
onPureCall()
{
    reportFatalError("pure virtual function call", true);
    std::abort();
}
//=============================================================================
static void
describeCppException(const EXCEPTION_RECORD* record, char* text, size_t textSize)
{
    std::snprintf(text, textSize, "uncaught C++ exception");
    if (record->NumberParameters < 4 || record->ExceptionInformation[1] == 0
        || record->ExceptionInformation[2] == 0 || record->ExceptionInformation[3] == 0) {
        return;
    }
    // The thrown object may be damaged: any fault here keeps the short text.
    __try {
        const char* object = reinterpret_cast<const char*>(record->ExceptionInformation[1]);
        const auto* thrown = reinterpret_cast<const ThrownInfo*>(record->ExceptionInformation[2]);
        const char* imageBase = reinterpret_cast<const char*>(record->ExceptionInformation[3]);
        const auto* types = reinterpret_cast<const ThrownCatchableTypeArray*>(
            imageBase + thrown->catchableTypeArray);
        char typeName[TEXT_SIZE / 2] = "";
        const char* message = nullptr;
        for (int k = 0; k < types->count; ++k) {
            const auto* type
                = reinterpret_cast<const ThrownCatchableType*>(imageBase + types->types[k]);
            const auto* descriptor
                = reinterpret_cast<const ThrownTypeDescriptor*>(imageBase + type->typeDescriptor);
            if (k == 0) {
                readableTypeName(descriptor->name, typeName, sizeof(typeName));
            }
            if (std::strcmp(descriptor->name, ".?AVexception@std@@") == 0) {
                const auto* base
                    = reinterpret_cast<const std::exception*>(object + type->memberDisplacement);
                message = base->what();
                break;
            }
        }
        if (message != nullptr) {
            std::snprintf(text, textSize, "uncaught C++ exception %s: %s", typeName, message);
        } else {
            std::snprintf(text, textSize, "uncaught C++ exception %s", typeName);
        }
    } __except (EXCEPTION_EXECUTE_HANDLER) {
        std::snprintf(text, textSize, "uncaught C++ exception");
    }
}
//=============================================================================
// std::terminate() reached without going through the exception filter, for
// example from a noexcept function: the exception is still the current one.
static void
describeCurrentException(char* text, size_t textSize)
{
    std::snprintf(text, textSize, "abort() or std::terminate() called");
    try {
        std::exception_ptr current = std::current_exception();
        if (current) {
            std::rethrow_exception(current);
        }
    } catch (const std::exception& e) {
        // typeid gives "class std::runtime_error"
        const char* typeName = typeid(e).name();
        const char* space = std::strchr(typeName, ' ');
        std::snprintf(text, textSize, "std::terminate() called after %s: %s",
            space != nullptr ? space + 1 : typeName, e.what());
    } catch (...) {
        std::snprintf(text, textSize, "std::terminate() called after an unknown exception");
    }
}
//=============================================================================
// ".?AVruntime_error@std@@" becomes "std::runtime_error"; other forms are kept as is.
static void
readableTypeName(const char* decorated, char* text, size_t textSize)
{
    std::snprintf(text, textSize, "%s", decorated);
    const bool isClassOrStruct
        = std::strncmp(decorated, ".?AV", 4) == 0 || std::strncmp(decorated, ".?AU", 4) == 0;
    const size_t length = std::strlen(decorated);
    if (!isClassOrStruct || length < 7 || std::strcmp(decorated + length - 2, "@@") != 0
        || std::strrchr(decorated, '?') != decorated + 1
        || std::strchr(decorated, '$') != nullptr) {
        return;
    }
    // Scopes are stored innermost first, separated by '@'.
    char scopes[TEXT_SIZE / 2];
    std::snprintf(scopes, sizeof(scopes), "%.*s", static_cast<int>(length - 6), decorated + 4);
    text[0] = '\0';
    for (;;) {
        char* separator = std::strrchr(scopes, '@');
        const char* scope = separator != nullptr ? separator + 1 : scopes;
        const size_t used = std::strlen(text);
        std::snprintf(text + used, textSize - used, "%s%s", used == 0 ? "" : "::", scope);
        if (separator == nullptr) {
            break;
        }
        *separator = '\0';
    }
}
//=============================================================================
static void
reportFatalError(const char* reason, bool withStack)
{
    // One report per process: the first handler reached tells the cause.
    if (alreadyReported.test_and_set()) {
        return;
    }
    char text[TEXT_SIZE + 128];
    std::snprintf(text, sizeof(text),
        "\nNelson: fatal error, the process stops.\n  reason: %s\n  thread: %lu\n", reason,
        static_cast<unsigned long>(GetCurrentThreadId()));
    writeText(text);
    if (withStack) {
        writeStack();
    }
}
//=============================================================================
static void
writeStack()
{
    void* frames[MAX_STACK_FRAMES];
    const USHORT count = RtlCaptureStackBackTrace(0, MAX_STACK_FRAMES, frames, nullptr);
    const DWORD byAddress
        = GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT;
    HMODULE reportModule = nullptr;
    GetModuleHandleExW(byAddress, reinterpret_cast<LPCWSTR>(&writeStack), &reportModule);
    writeText("  stack:\n");
    // The first frames are the ones of this report: they tell nothing.
    bool inReport = reportModule != nullptr;
    for (USHORT k = 0; k < count; ++k) {
        char line[TEXT_SIZE];
        HMODULE module = nullptr;
        const bool found
            = GetModuleHandleExW(byAddress, static_cast<LPCWSTR>(frames[k]), &module) != FALSE;
        if (inReport && found && module == reportModule) {
            continue;
        }
        inReport = false;
        if (!found || module == nullptr) {
            std::snprintf(line, sizeof(line), "    0x%p\n", frames[k]);
            writeText(line);
            continue;
        }
        char path[MAX_PATH] = "";
        GetModuleFileNameA(module, path, sizeof(path));
        const char* name = std::strrchr(path, '\\');
        name = name != nullptr ? name + 1 : path;
        size_t displacement = 0;
        const char* symbol = nearestExportedName(module, frames[k], &displacement);
        const size_t offset = static_cast<size_t>(
            static_cast<const char*>(frames[k]) - reinterpret_cast<const char*>(module));
        if (symbol != nullptr) {
            std::snprintf(line, sizeof(line), "    %s+0x%zx (near %.96s+0x%zx)\n", name, offset,
                symbol, displacement);
        } else {
            std::snprintf(line, sizeof(line), "    %s+0x%zx\n", name, offset);
        }
        writeText(line);
    }
}
//=============================================================================
// Closest exported function at or before the address, read from the export
// table: no symbol file is shipped, so the real function may be an internal one.
static const char*
nearestExportedName(HMODULE module, const void* address, size_t* displacement)
{
    const char* result = nullptr;
    __try {
        const char* base = reinterpret_cast<const char*>(module);
        const auto* dosHeader = reinterpret_cast<const IMAGE_DOS_HEADER*>(base);
        if (dosHeader->e_magic != IMAGE_DOS_SIGNATURE) {
            return nullptr;
        }
        const auto* ntHeaders
            = reinterpret_cast<const IMAGE_NT_HEADERS*>(base + dosHeader->e_lfanew);
        const IMAGE_DATA_DIRECTORY& directory
            = ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
        if (directory.VirtualAddress == 0 || directory.Size == 0) {
            return nullptr;
        }
        const auto* exports
            = reinterpret_cast<const IMAGE_EXPORT_DIRECTORY*>(base + directory.VirtualAddress);
        const auto* functions = reinterpret_cast<const DWORD*>(base + exports->AddressOfFunctions);
        const auto* names = reinterpret_cast<const DWORD*>(base + exports->AddressOfNames);
        const auto* ordinals = reinterpret_cast<const WORD*>(base + exports->AddressOfNameOrdinals);
        const size_t target = static_cast<size_t>(static_cast<const char*>(address) - base);
        size_t best = 0;
        for (DWORD k = 0; k < exports->NumberOfNames; ++k) {
            const size_t function = functions[ordinals[k]];
            // A forwarded export points inside the export table itself.
            const bool forwarded = function >= directory.VirtualAddress
                && function < static_cast<size_t>(directory.VirtualAddress) + directory.Size;
            if (!forwarded && function <= target && function >= best) {
                best = function;
                result = base + names[k];
            }
        }
        if (result != nullptr) {
            *displacement = target - best;
        }
    } __except (EXCEPTION_EXECUTE_HANDLER) {
        result = nullptr;
    }
    return result;
}
//=============================================================================
// Straight to the handle: the C streams may be the part that is broken.
static void
writeText(const char* text)
{
    HANDLE errorOutput = GetStdHandle(STD_ERROR_HANDLE);
    if (errorOutput == nullptr || errorOutput == INVALID_HANDLE_VALUE) {
        return;
    }
    DWORD written = 0;
    WriteFile(errorOutput, text, static_cast<DWORD>(std::strlen(text)), &written, nullptr);
}
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
