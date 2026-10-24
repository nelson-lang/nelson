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
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Language adapter for block codegen emitters written ONCE and registered
    // for both targets:
    //
    //   static BlockCodegenTemplate makeXCodegen(CodegenLang L) { ... }
    //   BlockCodegenTemplate getCodeGenCX()    { return makeXCodegen({ false }); }
    //   BlockCodegenTemplate getCodeGenRustX() { return makeXCodegen({ true }); }
    //
    // Each helper emits the same text as the per-language emitters it replaces
    // (the golden fixtures check it).
    //=============================================================================
    struct CodegenLang
    {
        bool rust = false;

        // State field access: "s-><field>" (C) / "s.<field>" (Rust).
        std::string
        sref(const std::string& field) const
        {
            return (rust ? "s." : "s->") + field;
        }
        // Zero literal: "0.0" / "0.0_f64".
        std::string
        zero() const
        {
            return rust ? "0.0_f64" : "0.0";
        }
        // Local declaration: "double n = init;" / "let mut n = init;".
        std::string
        declLocal(const std::string& name, const std::string& init) const
        {
            return rust ? ("let mut " + name + " = " + init + ";")
                        : ("double " + name + " = " + init + ";");
        }
        // Conditional expression: "(c) ? (a) : (b)" / "if c { a } else { b }".
        std::string
        ternary(const std::string& c, const std::string& a, const std::string& b) const
        {
            return rust ? ("if " + c + " { " + a + " } else { " + b + " }")
                        : ("(" + c + ") ? (" + a + ") : (" + b + ")");
        }
        // Unary math call: "fn(e)" / "libm::fn(e)".
        std::string
        call(const char* fn, const std::string& e) const
        {
            return (rust ? ("libm::" + std::string(fn)) : std::string(fn)) + "(" + e + ")";
        }
        // MechEmitLang conventions (Rust prefers the method forms here).
        std::string
        tanh(const std::string& e) const
        {
            return rust ? ("(" + e + ").tanh()") : ("tanh(" + e + ")");
        }
        std::string
        fabs(const std::string& e) const
        {
            return rust ? ("libm::fabs(" + e + ")") : ("fabs(" + e + ")");
        }
        std::string
        fmax(const std::string& x, const std::string& y) const
        {
            return rust ? ("(" + x + ").max(" + y + ")") : ("fmax(" + x + ", " + y + ")");
        }
        std::string
        fmin(const std::string& x, const std::string& y) const
        {
            return rust ? ("(" + x + ").min(" + y + ")") : ("fmin(" + x + ", " + y + ")");
        }
    };
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
