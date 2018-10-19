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
#include "BuiltInFunctionDef.hpp"
#include "EvaluateBuiltinCatchRuntimeException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BuiltInFunctionDef::BuiltInFunctionDef()
{
    fileName.clear();
    retCount = 0;
    argCount = 0;
    fptr = nullptr;
}
//=============================================================================
BuiltInFunctionDef::~BuiltInFunctionDef() {}
//=============================================================================
ArrayOfVector
BuiltInFunctionDef::evaluateFunction(Evaluator* eval, ArrayOfVector& inputs, int nargout)
{
    ArrayOfVector outputs;
    eval->pushDebug(name, std::string("built-in ") + this->name);
    size_t stackDepth = eval->cstack.size();
    try {
        outputs = EvaluateBuiltinCatchRuntimeException(eval, fptr, inputs, nargout);
        while (eval->cstack.size() > stackDepth) {
            eval->cstack.pop_back();
        }
        eval->popDebug();
        return outputs;
    } catch (const Exception&) {
        while (eval->cstack.size() > stackDepth) {
            eval->cstack.pop_back();
        }
        eval->popDebug();
        throw;
    }
}
//=============================================================================
void
BuiltInFunctionDef::printMe(Interface* io)
{}
//=============================================================================
}
//=============================================================================
