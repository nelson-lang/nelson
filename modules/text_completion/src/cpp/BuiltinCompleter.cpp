//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
BuiltinCompleter(std::wstring prefix)
{
    wstringVector res;
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
        wstringVector builtin = WhatListOfBuiltin(eval, true, true);
        for (size_t k = 0; k < builtin.size(); k++) {
            if (boost::algorithm::starts_with(builtin[k], prefix)) {
                res.push_back(builtin[k]);
            }
        }
    }
    return res;
}
//=============================================================================
};
//=============================================================================
