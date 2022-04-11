//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ModulesHelpers.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
ConstructBinariesPath(const std::wstring& modulerootpath)
{
    std::wstring binariespath;
#ifdef _MSC_VER
#ifdef _WIN64
    binariespath = modulerootpath + std::wstring(L"/bin/x64");
#else
    binariespath = modulerootpath + std::wstring(L"/bin/win32");
#endif
#else
#ifdef __APPLE__
    binariespath = modulerootpath + std::wstring(L"/bin/macOS");
#else
    binariespath = modulerootpath + std::wstring(L"/bin/linux");
#endif
#endif
    return binariespath;
}
//=============================================================================
std::wstring
ConstructDynamicLibraryName(const std::wstring& moduleshortname)
{
    std::wstring name = moduleshortname;
    name[0] = towupper(name[0]);
    std::wstring libname = std::wstring(L"libnls") + name + std::wstring(L"_builtin");
#ifdef _MSC_VER
    libname = libname + std::wstring(L".dll");
#else
#ifdef __APPLE__
    libname = libname + std::wstring(L".dylib");
#else
    libname = libname + std::wstring(L".so");
#endif
#endif
    return libname;
}
//=============================================================================
std::wstring
ConstructEtcName(const std::wstring& modulerootpath, const std::wstring& moduleshortname)
{
    std::wstring etcpath;
    if (moduleshortname.empty()) {
        etcpath = modulerootpath + L"/etc";
    } else {
        etcpath = modulerootpath + L"/modules/" + moduleshortname + L"/etc";
    }
    return etcpath;
}
//=============================================================================
std::wstring
ConstructRootName(const std::wstring& modulerootpath, const std::wstring& moduleshortname)
{
    std::wstring rootpath;
    if (moduleshortname.empty()) {
        rootpath = modulerootpath + L"/";
    } else {
        rootpath = modulerootpath + L"/modules/" + moduleshortname + L"/";
    }
    return rootpath;
}
//=============================================================================
std::wstring
ConstructScriptName(const std::wstring& modulerootpath, const std::wstring& moduleshortname)
{
    std::wstring scriptspath;
    if (moduleshortname.empty()) {
        scriptspath = modulerootpath + L"/functions";
    } else {
        scriptspath = modulerootpath + L"/modules/" + moduleshortname + L"/functions";
    }
    return scriptspath;
}
//=============================================================================
std::wstring
ConstructDynamicLibraryFullname(
    const std::wstring& modulerootpath, const std::wstring& moduleshortname)
{
    return ConstructBinariesPath(modulerootpath) + L"/"
        + ConstructDynamicLibraryName(moduleshortname);
}
} // namespace Nelson
//=============================================================================
