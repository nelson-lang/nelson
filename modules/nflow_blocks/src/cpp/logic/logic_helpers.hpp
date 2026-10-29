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
#include "SimEngineTypes.hpp"
#include "FieldNames.hpp"
#include <string>
//=============================================================================
// Shared helpers for the "builtin.logic" block handlers.
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    inline bool
    compareValues(double lhs, double rhs, const std::string& op)
    {
        if (op == "gt" || op == ">") {
            return lhs > rhs;
        }
        if (op == "le" || op == "<=") {
            return lhs <= rhs;
        }
        if (op == "lt" || op == "<") {
            return lhs < rhs;
        }
        if (op == "eq" || op == "==") {
            return lhs == rhs;
        }
        if (op == "ne" || op == "!=" || op == "~=") {
            // '~=' is how Nelson spells inequality; without it the operator falls
            // through to the '>=' default.
            return lhs != rhs;
        }
        return lhs >= rhs;
    }
    //=============================================================================
    inline std::string
    operatorParam(const Block& b, const char* key, const std::string& fallback)
    {
        if (asJson(b.params).contains(key) && asJson(b.params)[key].is_string()) {
            return asJson(b.params)[key].get<std::string>();
        }
        if (asJson(b.params).contains(nflow::kCondition) && asJson(b.params)[nflow::kCondition].is_string()) {
            return asJson(b.params)[nflow::kCondition].get<std::string>();
        }
        return fallback;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
