//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Validators.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Evaluator* _eval = nullptr;
//=============================================================================
void
setEvaluator(Evaluator* eval)
{
    _eval = eval;
}
//=============================================================================
static ArrayOf
callUnaryFunction(const ArrayOf& arg, const std::string& functionName)
{
    Context* context = _eval->getContext();
    FunctionDef* funcDef = nullptr;
    if (context->lookupFunction(functionName, funcDef)) {
        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
            ArrayOfVector argIn(arg);
            ArrayOfVector argOut = funcDef->evaluateFunction(_eval, argIn, 1);
            if (argOut.empty()) {
                return ArrayOf();
            }
            return argOut[0];
        }
    }
    return ArrayOf();
}
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFinite(const ArrayOf& arg, bool asCaller)
{
    bool asLogicalIsAllFinite = false;
    ArrayOf isFinite = callUnaryFunction(arg, "isfinite");
    ArrayOf isAllFinite = callUnaryFunction(isFinite, "all");
    asLogicalIsAllFinite = isAllFinite.getContentAsLogicalScalar();
    if (!asLogicalIsAllFinite) {
        std::wstring msg = _W("Value must be finite.");
        std::wstring id = _W("Nelson:validators:mustBeFinite");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeLogical(const ArrayOf& arg, bool asCaller)
{
    std::string name = ClassName(arg);
    ArrayOf isEmptyArrayOf = callUnaryFunction(arg, "isempty");
    bool isEmpty = isEmptyArrayOf.getContentAsLogicalScalar();
    if (!isEmpty) {
        bool isLogical = (arg.isLogical() || name == "logical");
        if (!isLogical) {
            std::wstring msg = _W("Value must be logical.");
            std::wstring id = _W("Nelson:validators:mustBeLogical");
            Error(msg, id, asCaller);
        }
    }
}
//=============================================================================
void
mustBeLogicalScalar(const ArrayOf& arg, bool asCaller)
{
    std::string name = ClassName(arg);
    bool isLogical = (arg.isLogical() || name == "logical");
    ArrayOf isScalarArrayOf = callUnaryFunction(arg, "isscalar");
    bool isScalar = isScalarArrayOf.getContentAsLogicalScalar();
    bool isLogicalScalar = isLogical && isScalar;
    if (!isLogicalScalar) {
        std::wstring msg = _W("Value must be logical scalar.");
        std::wstring id = _W("Nelson:validators:mustBeLogicalScalar");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
