//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "logic/logic_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerLogicHandlers(BlockRegistry& reg)
    {
        reg.registerHandler("and", handleAnd, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("or", handleOr, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("xor", handleXor, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("not", handleNot, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler(
            "compareToConstant", handleCompareToConstant, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler(
            "compareToZero", handleCompareToZero, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler(
            "intervalTest", handleIntervalTest, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("intervalTestDynamic", handleIntervalTestDynamic, { Phase::ALGEBRAIC },
            "builtin.logic");
        reg.registerHandler(
            "bitwiseOperator", handleBitwiseOperator, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("bitSet", handleBitSet, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("bitClear", handleBitClear, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler(
            "extractBits", handleExtractBits, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler(
            "shiftArithmetic", handleShiftArithmetic, { Phase::ALGEBRAIC }, "builtin.logic");
        BlockMetadata combLogicMeta;
        combLogicMeta.directFeedthrough = true;
        combLogicMeta.acceptsVectorInputs = true; // the N-bit index is a vector input
        combLogicMeta.resolveDims = [](const Block&, const ValMap&, const std::vector<PortSig>&,
                                        std::vector<PortSig>& outSigs, std::string&) -> bool {
            outSigs[0].setVector(1);
            outSigs[0].type = SigType::Double;
            return true;
        };
        reg.registerHandler("combinatorialLogic", handleCombinatorialLogic, { Phase::ALGEBRAIC },
            "builtin.logic", combLogicMeta);
        reg.registerCodegen("combinatorialLogic",
            BlockRegistry::BlockCodegenPair {
                getCodeGenCCombinatorialLogic(), getCodeGenRustCombinatorialLogic() });
        reg.registerHandler(
            "relationalOperator", handleRelationalOperator, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler(
            "logicalOperator", handleLogicalOperator, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("switchCase", handleSwitchCase, { Phase::ALGEBRAIC }, "builtin.logic");
        reg.registerHandler("if", handleIf, { Phase::ALGEBRAIC }, "builtin.logic");

        // Code-generation templates (other blocks keep native emission paths).
        reg.registerCodegen("compareToConstant", "c", getCodeGenCCompareToConstant());
        reg.registerCodegen("compareToConstant", "rust", getCodeGenRustCompareToConstant());
        reg.registerCodegen("compareToZero", "c", getCodeGenCCompareToZero());
        reg.registerCodegen("compareToZero", "rust", getCodeGenRustCompareToZero());
        reg.registerCodegen("intervalTest", "c", getCodeGenCIntervalTest());
        reg.registerCodegen("intervalTest", "rust", getCodeGenRustIntervalTest());
        reg.registerCodegen("intervalTestDynamic", "c", getCodeGenCIntervalTestDynamic());
        reg.registerCodegen("intervalTestDynamic", "rust", getCodeGenRustIntervalTestDynamic());
        reg.registerCodegen("bitwiseOperator", "c", getCodeGenCBitwiseOperator());
        reg.registerCodegen("bitwiseOperator", "rust", getCodeGenRustBitwiseOperator());
        reg.registerCodegen("bitSet", "c", getCodeGenCBitSet());
        reg.registerCodegen("bitSet", "rust", getCodeGenRustBitSet());
        reg.registerCodegen("bitClear", "c", getCodeGenCBitClear());
        reg.registerCodegen("bitClear", "rust", getCodeGenRustBitClear());
        reg.registerCodegen("switchCase", "c", getCodeGenCSwitchCase());
        reg.registerCodegen("switchCase", "rust", getCodeGenRustSwitchCase());
        reg.registerCodegen("if", "c", getCodeGenCIf());
        reg.registerCodegen("if", "rust", getCodeGenRustIf());
        reg.registerCodegen("extractBits", "c", getCodeGenCExtractBits());
        reg.registerCodegen("extractBits", "rust", getCodeGenRustExtractBits());
        reg.registerCodegen("shiftArithmetic", "c", getCodeGenCShiftArithmetic());
        reg.registerCodegen("shiftArithmetic", "rust", getCodeGenRustShiftArithmetic());
        reg.registerCodegen("relationalOperator", "c", getCodeGenCRelationalOperator());
        reg.registerCodegen("relationalOperator", "rust", getCodeGenRustRelationalOperator());
        reg.registerCodegen("logicalOperator", "c", getCodeGenCLogicalOperator());
        reg.registerCodegen("logicalOperator", "rust", getCodeGenRustLogicalOperator());
        reg.registerCodegen("and", "c", getCodeGenCAnd());
        reg.registerCodegen("and", "rust", getCodeGenRustAnd());
        reg.registerCodegen("or", "c", getCodeGenCOr());
        reg.registerCodegen("or", "rust", getCodeGenRustOr());
        reg.registerCodegen("xor", "c", getCodeGenCXor());
        reg.registerCodegen("xor", "rust", getCodeGenRustXor());
        reg.registerCodegen("not", "c", getCodeGenCNot());
        reg.registerCodegen("not", "rust", getCodeGenRustNot());
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
