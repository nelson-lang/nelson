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
#include <string>
#include <vector>
#include "Bytecode.hpp"
#include "ConstantPool.hpp"
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class FunctionDef;
//=============================================================================
class NLSINTERPRETER_IMPEXP BytecodeChunk
{
public:
    std::wstring sourcePath;
    std::string functionName;
    FunctionDef* selfFunction = nullptr;
    bool isScript = false;
    bool mayCreateNamedFunctionHandle = false;

    std::vector<Instruction> code;
    ConstantPool constants;
    std::vector<std::string> names;

    uint16_t nLocals = 0;
    uint16_t nLoopSlots = 0;
    uint16_t maxStack = 0;

    stringVector argNames;
    stringVector retNames;
    stringVector capturedNames;
    stringVector localNames;
    std::vector<uint8_t> localAssigned;
    std::vector<uint16_t> retSlots;

    std::vector<SourceSpan> spans;

    bool callerContextSyncCacheValid = false;
    bool callerContextSyncRequired = true;
    bool callerContextRefreshCacheValid = false;
    bool callerContextRefreshRequired = true;

    uint32_t
    emit(OpCode op, uint8_t flags = 0, uint16_t A = 0, uint16_t B = 0, uint16_t C = 0);

    void
    patchJump(uint32_t instrIdx, uint16_t target);

    void
    recordSpan(uint32_t instrIdx, uint32_t line, uint16_t col = 0, uint16_t endLine = 0);

    const SourceSpan*
    spanForPC(uint32_t pc) const;

    std::string
    disassemble() const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
