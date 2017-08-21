//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "nlsInterpreter_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
    NLSINTERPRETER_IMPEXP ArrayOfVector CallOperatorFunction(Evaluator *eval, const std::string &functionName, ArrayOfVector ArgsIn, size_t nLhs);
    NLSINTERPRETER_IMPEXP ArrayOfVector CallOperatorFunction(Evaluator *eval, const std::string &functionName, ArrayOf a, size_t nLhs);
    NLSINTERPRETER_IMPEXP ArrayOfVector CallOperatorFunction(Evaluator *eval, const std::string &functionName, ArrayOf a, ArrayOf b, size_t nLhs);
    NLSINTERPRETER_IMPEXP ArrayOfVector CallOperatorFunction(Evaluator *eval, const std::string &functionName, ArrayOf a, ArrayOf b, ArrayOf c, size_t nLhs);
}
//=============================================================================