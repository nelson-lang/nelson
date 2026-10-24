//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared helpers for the exact-64 (int64/uint64) typed code-generation
// emitters (BlockCodegenTemplate::emitStepTyped). The C generator emits the
// nflow_i64_* / nflow_u64_* helper preamble whenever an exact-64 port
// exists; Rust uses the core saturating_/wrapping_ intrinsics.
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <limits>
#include "BlockRegistry.hpp"
#include "SimEngineTypes.hpp"
#include "FieldNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    inline bool
    codegenIsU64(const std::string& typeName)
    {
        return typeName == "uint64";
    }
    //=============================================================================
    inline bool
    codegenIsExact64(const std::string& typeName)
    {
        return typeName == "int64" || typeName == "uint64";
    }
    //=============================================================================
    // Small integer types quantized by the GENERATORS around the double
    // emission (the simulator computes them in the double lane and quantizes
    // every produced element). Returns false for anything else.
    inline bool
    codegenSmallIntRange(const std::string& typeName, double& lo, double& hi)
    {
        if (typeName == "int8") {
            lo = -128.0;
            hi = 127.0;
        } else if (typeName == "uint8") {
            lo = 0.0;
            hi = 255.0;
        } else if (typeName == "int16") {
            lo = -32768.0;
            hi = 32767.0;
        } else if (typeName == "uint16") {
            lo = 0.0;
            hi = 65535.0;
        } else if (typeName == "int32") {
            lo = -2147483648.0;
            hi = 2147483647.0;
        } else if (typeName == "uint32") {
            lo = 0.0;
            hi = 4294967295.0;
        } else {
            return false;
        }
        return true;
    }
    //=============================================================================
    // SaturateOnOverflow block parameter (default: saturate).
    inline bool
    codegenSaturates(const BlockCodegenArgs& a)
    {
        if (a.params && a.params->contains(nflow::kSaturateOnOverflow)
            && (*a.params)[nflow::kSaturateOnOverflow].is_boolean()) {
            return (*a.params)[nflow::kSaturateOnOverflow].get<bool>();
        }
        return true;
    }
    //=============================================================================
    // Exact-64 literal for the target language ("c" or "rust"). INT64_MIN
    // cannot be written as a plain literal in C.
    inline std::string
    codegenI64Literal(long long v, bool uns, bool cLang)
    {
        if (uns) {
            return std::to_string(static_cast<unsigned long long>(v)) + (cLang ? "ULL" : "_u64");
        }
        if (v == std::numeric_limits<long long>::min()) {
            return cLang ? "INT64_MIN" : "i64::MIN";
        }
        return std::to_string(v) + (cLang ? "LL" : "_i64");
    }
    //=============================================================================
    // Saturating / wrapping binary operation on two exact-64 expressions.
    // op is one of "add", "sub", "mul".
    inline std::string
    codegenI64BinOp(const std::string& op, const std::string& lhs, const std::string& rhs, bool uns,
        bool saturate, bool cLang)
    {
        if (cLang) {
            if (saturate) {
                return std::string("nflow_") + (uns ? "u64" : "i64") + "_sat_" + op + "(" + lhs
                    + ", " + rhs + ")";
            }
            // Wrap: unsigned arithmetic is well defined; signed wraps through
            // an unsigned round-trip.
            const char* cop = (op == "add") ? " + " : (op == "sub") ? " - " : " * ";
            if (uns) {
                return "(" + lhs + cop + rhs + ")";
            }
            return "(int64_t)((uint64_t)" + lhs + cop + "(uint64_t)" + rhs + ")";
        }
        const std::string method = std::string(saturate ? "saturating_" : "wrapping_") + op;
        return lhs + "." + method + "(" + rhs + ")";
    }
    //=============================================================================
    // Boolean comparison emission shared by the comparison blocks (the
    // output stays a double-backed boolean; only the operand types differ).
    inline void
    codegenEmitTypedCompare(const BlockCodegenArgs& a, const std::string& op,
        const std::string& lhs, const std::string& rhs, bool cLang)
    {
        std::string cmp;
        if (op == "gt" || op == ">") {
            cmp = lhs + " > " + rhs;
        } else if (op == "le" || op == "<=") {
            cmp = lhs + " <= " + rhs;
        } else if (op == "lt" || op == "<") {
            cmp = lhs + " < " + rhs;
        } else if (op == "eq" || op == "==") {
            cmp = lhs + " == " + rhs;
        } else if (op == "ne" || op == "!=" || op == "~=") {
            cmp = lhs + " != " + rhs;
        } else {
            cmp = lhs + " >= " + rhs;
        }
        if (cLang) {
            a.line("out_" + a.id + " = (" + cmp + ") ? 1.0 : 0.0;");
        } else {
            a.line("out_" + a.id + " = if " + cmp + " { 1.0_f64 } else { 0.0_f64 };");
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
