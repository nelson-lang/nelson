//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BuiltinCompleter.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "What.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
BuiltinCompleter(const std::wstring& prefix)
{
    wstringVector res;
    auto* eval = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    if (eval) {
        wstringVector builtin = WhatListOfBuiltin(eval, true, true);
        for (const auto& k : builtin) {
            if (boost::algorithm::starts_with(k, prefix)) {
                res.push_back(k);
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
