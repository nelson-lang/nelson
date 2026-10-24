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
#include <atomic>
#include <cstdint>
#include <vector>
#include <unordered_map>
#include <string>
#include "BuiltInFunctionDef.hpp"
#include "nlsInterpreter_exports.h"
#include "NelsonGateway.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP BuiltInFunctionDefManager
{
public:
    static BuiltInFunctionDefManager*
    getInstance();
    bool
    add(const std::string& name, void* fptr, int argc_in, int argc_out,
        const std::wstring& dynlibname, const std::wstring& modulename, size_t builtinPrototype,
        bool interleavedComplex,
        FunctionOverloadAutoMode builtinOverloadAutoMode = NLS_OVERLOAD_AUTO_ON);
    bool
    addLazyGateway(const std::string& name, int argc_in, int argc_out,
        const std::wstring& dynlibname, const std::wstring& modulename, size_t builtinPrototype,
        FunctionOverloadAutoMode builtinOverloadAutoMode = NLS_OVERLOAD_AUTO_ON);
    bool
    setCallerContextAccess(ptrBuiltin fptr, bool callerContextAccess);

    bool
    remove(const std::string& name);
    bool
    removeByFilename(const std::wstring& dynlibname);
    bool
    remove(FunctionDefPtr ptr);
    bool
    remove(BuiltInFunctionDef* ptr);
    bool
    remove(ptrBuiltin fptr);
    bool
    removeAll();

    void
    destroy();

    std::vector<FunctionDefPtr>
    getTable();
    stringVector
    getNameList();
    bool
    find(const std::string& name, FunctionDefPtr& ptr);
    bool
    find(const std::string& name, wstringVector& paths);
    bool
    find(const std::string& name, std::wstring& path);

    // Incremented whenever the set of registered builtin names changes; lets
    // clients (completion) cache derived lists until the table really moves.
    uint64_t
    generation() const
    {
        return _generation.load(std::memory_order_relaxed);
    }

private:
    bool
    add(FunctionDefPtr ptr);

    void
    bumpGeneration()
    {
        _generation.fetch_add(1, std::memory_order_relaxed);
    }

    BuiltInFunctionDefManager();
    static BuiltInFunctionDefManager* m_pInstance;
    std::vector<FunctionDefPtr> builtinVector;
    std::unordered_map<std::string, FunctionDefPtr> builtinHashMap;
    std::atomic<uint64_t> _generation { 0 };
};
//=============================================================================
} // namespace Nelson
//=============================================================================
