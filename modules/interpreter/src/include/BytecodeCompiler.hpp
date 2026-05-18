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
#include <memory>
#include "AbstractSyntaxTree.hpp"
#include "BytecodeChunk.hpp"
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP BytecodeCompiler
{
public:
    static std::unique_ptr<BytecodeChunk>
    compileFunction(AbstractSyntaxTreePtr body, const std::string& functionName,
        const std::wstring& sourcePath, const stringVector& argNames, const stringVector& retNames);

    static std::unique_ptr<BytecodeChunk>
    compileScript(AbstractSyntaxTreePtr body, const std::wstring& sourcePath);

    static std::unique_ptr<BytecodeChunk>
    compileAnonymous(AbstractSyntaxTreePtr body, const stringVector& capturedNames,
        const stringVector& argNames, const stringVector& retNames = stringVector());
};
//=============================================================================
} // namespace Nelson
//=============================================================================
