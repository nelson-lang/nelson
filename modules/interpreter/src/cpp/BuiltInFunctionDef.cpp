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
#include "BuiltInFunctionDef.hpp"
#include "EvaluateBuiltinCatchRuntimeException.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "NelsonGateway.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BuiltInFunctionDef::BuiltInFunctionDef()
{
    fileName.clear();
    retCount = 0;
    argCount = 0;
    fptr = nullptr;
    builtinPrototype = (size_t)BUILTIN_PROTOTYPE::CPP_BUILTIN;
}
//=============================================================================
BuiltInFunctionDef::~BuiltInFunctionDef() = default;
//=============================================================================
ArrayOfVector
BuiltInFunctionDef::evaluateFunction(Evaluator* eval, ArrayOfVector& inputs, int nargout)
{
    ArrayOfVector outputs;
    eval->pushDebug(name, std::string("built-in ") + this->name);
    size_t stackDepth = eval->cstack.size();
    uint64 tic = 0;
    try {
        tic = Profiler::getInstance()->tic();
        outputs = EvaluateBuiltinCatchRuntimeException(
            eval, fptr, this->name, inputs, nargout, builtinPrototype);
        if (tic != 0) {
            internalProfileFunction stack = computeProfileStack(eval, this->name, this->fileName);
            Profiler::getInstance()->toc(tic, stack);
        }
        while (eval->cstack.size() > stackDepth) {
            eval->cstack.pop_back();
        }
        eval->popDebug();
        return outputs;
    } catch (const Exception&) {
        if (tic != 0) {
            internalProfileFunction stack = computeProfileStack(eval, this->name, this->fileName);
            Profiler::getInstance()->toc(tic, stack);
        }
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
} // namespace Nelson
//=============================================================================
