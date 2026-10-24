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
#include <cmath>
#include <cstdint>
#include <string>
#include "ScalarMath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class ScalarMathBuiltinKind : uint8_t
{
    None,
    Abs,
    Atan,
    Atan2,
    Ceil,
    Cos,
    Exp,
    Fix,
    Floor,
    Gamma,
    Log,
    Log2,
    Log10,
    Maximum,
    Minimum,
    Mod,
    Rem,
    Round,
    Sin,
    Sqrt,
    Tan
};
//=============================================================================
inline ScalarMathBuiltinKind
scalarMathBuiltinKind(const std::string& name, uint16_t inputCount)
{
    if (inputCount == 2) {
        if (name == "atan2") {
            return ScalarMathBuiltinKind::Atan2;
        }
        if (name == "max") {
            return ScalarMathBuiltinKind::Maximum;
        }
        if (name == "min") {
            return ScalarMathBuiltinKind::Minimum;
        }
        if (name == "mod") {
            return ScalarMathBuiltinKind::Mod;
        }
        if (name == "rem") {
            return ScalarMathBuiltinKind::Rem;
        }
        return ScalarMathBuiltinKind::None;
    }
    if (inputCount != 1) {
        return ScalarMathBuiltinKind::None;
    }
    switch (name.size()) {
    case 3:
        if (name == "abs") {
            return ScalarMathBuiltinKind::Abs;
        }
        if (name == "cos") {
            return ScalarMathBuiltinKind::Cos;
        }
        if (name == "exp") {
            return ScalarMathBuiltinKind::Exp;
        }
        if (name == "fix") {
            return ScalarMathBuiltinKind::Fix;
        }
        if (name == "log") {
            return ScalarMathBuiltinKind::Log;
        }
        if (name == "sin") {
            return ScalarMathBuiltinKind::Sin;
        }
        if (name == "tan") {
            return ScalarMathBuiltinKind::Tan;
        }
        break;
    case 4:
        if (name == "atan") {
            return ScalarMathBuiltinKind::Atan;
        }
        if (name == "ceil") {
            return ScalarMathBuiltinKind::Ceil;
        }
        if (name == "log2") {
            return ScalarMathBuiltinKind::Log2;
        }
        if (name == "sqrt") {
            return ScalarMathBuiltinKind::Sqrt;
        }
        break;
    case 5:
        if (name == "floor") {
            return ScalarMathBuiltinKind::Floor;
        }
        if (name == "gamma") {
            return ScalarMathBuiltinKind::Gamma;
        }
        if (name == "log10") {
            return ScalarMathBuiltinKind::Log10;
        }
        if (name == "round") {
            return ScalarMathBuiltinKind::Round;
        }
        break;
    default:
        break;
    }
    return ScalarMathBuiltinKind::None;
}
//=============================================================================
inline bool
isScalarMathBuiltinCandidate(ScalarMathBuiltinKind kind)
{
    return kind != ScalarMathBuiltinKind::None;
}
//=============================================================================
inline bool
evaluateUnaryScalarMathBuiltin(ScalarMathBuiltinKind kind, double value, double& result)
{
    switch (kind) {
    case ScalarMathBuiltinKind::Abs:
        result = std::abs(value);
        return true;
    case ScalarMathBuiltinKind::Atan:
        result = std::atan(value);
        return true;
    case ScalarMathBuiltinKind::Ceil:
        result = std::isfinite(value) ? std::ceil(value) : value;
        return true;
    case ScalarMathBuiltinKind::Cos:
        result = std::cos(value);
        return true;
    case ScalarMathBuiltinKind::Exp:
        result = std::exp(value);
        return true;
    case ScalarMathBuiltinKind::Fix:
        result = std::isfinite(value) ? std::trunc(value) : value;
        return true;
    case ScalarMathBuiltinKind::Floor:
        result = std::isfinite(value) ? std::floor(value) : value;
        return true;
    case ScalarMathBuiltinKind::Gamma:
        result = NelsonScalarGamma(value);
        return true;
    case ScalarMathBuiltinKind::Log:
        if (value < 0) {
            return false;
        }
        result = std::log(value);
        return true;
    case ScalarMathBuiltinKind::Log2:
        if (value < 0) {
            return false;
        }
        result = std::log(value) / std::log(2.0);
        return true;
    case ScalarMathBuiltinKind::Log10:
        if (value < 0) {
            return false;
        }
        result = std::log10(value);
        return true;
    case ScalarMathBuiltinKind::Round:
        result = std::isfinite(value) ? std::round(value) : value;
        return true;
    case ScalarMathBuiltinKind::Sin:
        result = std::sin(value);
        return true;
    case ScalarMathBuiltinKind::Sqrt:
        if (value < 0) {
            return false;
        }
        result = std::sqrt(value);
        return true;
    case ScalarMathBuiltinKind::Tan:
        result = std::tan(value);
        return true;
    default:
        return false;
    }
}
//=============================================================================
inline bool
evaluateBinaryScalarMathBuiltin(ScalarMathBuiltinKind kind, double lhs, double rhs, double& result)
{
    switch (kind) {
    case ScalarMathBuiltinKind::Atan2:
        result = std::atan2(lhs, rhs);
        return true;
    case ScalarMathBuiltinKind::Maximum:
        result = NelsonScalarMaximum(lhs, rhs);
        return true;
    case ScalarMathBuiltinKind::Minimum:
        result = NelsonScalarMinimum(lhs, rhs);
        return true;
    case ScalarMathBuiltinKind::Mod:
        result = NelsonScalarModulo(lhs, rhs);
        return true;
    case ScalarMathBuiltinKind::Rem:
        result = NelsonScalarRemainder(lhs, rhs);
        return true;
    default:
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
