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
#include "MxVariables.h"
#include "MxCall.h"
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "MxArrayOf.hpp"
//=============================================================================
static mxArray*
mexGetVariableCommon(const char* workspace, const char* varname, bool interleavedComplex)
{
    Nelson::Evaluator* mainEvaluator = (Nelson::Evaluator*)mexGetEvaluator();
    if (mainEvaluator != nullptr) {
        Nelson::Context* context = mainEvaluator->getContext();
        if (context != nullptr) {
            Nelson::Scope* scope = nullptr;
            if (strcmp(workspace, "base") == 0) {
                scope = context->getBaseScope();
            } else if (strcmp(workspace, "caller") == 0) {
                scope = context->getCallerScope();
            } else if (strcmp(workspace, "global") == 0) {
                scope = context->getGlobalScope();
            }
            if (scope != nullptr) {
                Nelson::ArrayOf variable;
                if (scope->lookupVariable(varname, variable)) {
                    return Nelson::ArrayOfToMxArray(variable, interleavedComplex);
                }
            }
        }
    }
    return nullptr;
}
//=============================================================================
mxArray*
mexGetVariableSeparatedComplex(const char* workspace, const char* varname)
{
    return mexGetVariableCommon(workspace, varname, false);
}
//=============================================================================
mxArray*
mexGetVariableInterleavedComplex(const char* workspace, const char* varname)
{
    return mexGetVariableCommon(workspace, varname, true);
}
//=============================================================================
static mxArray* ptrMexGetVariable = nullptr;
//=============================================================================
const mxArray*
mexGetVariablePtrSeparatedComplex(const char* workspace, const char* varname)
{
    if (ptrMexGetVariable != nullptr) {
        mxDestroyArray(ptrMexGetVariable);
    }
    ptrMexGetVariable = mexGetVariableCommon(workspace, varname, false);
    return ptrMexGetVariable;
}
//=============================================================================
const mxArray*
mexGetVariablePtrInterleavedComplex(const char* workspace, const char* varname)
{
    if (ptrMexGetVariable != nullptr) {
        mxDestroyArray(ptrMexGetVariable);
    }
    ptrMexGetVariable = mexGetVariableCommon(workspace, varname, true);
    return ptrMexGetVariable;
}
//=============================================================================
int
mexPutVariable(const char* workspace, const char* varname, const mxArray* pm)
{
    int ret = 1;
    Nelson::Evaluator* mainEvaluator = (Nelson::Evaluator*)mexGetEvaluator();
    if (mainEvaluator != nullptr) {
        Nelson::Context* context = mainEvaluator->getContext();
        if (context != nullptr) {
            Nelson::Scope* scope = nullptr;
            if (strcmp(workspace, "base") == 0) {
                scope = context->getBaseScope();
            } else if (strcmp(workspace, "caller") == 0) {
                scope = context->getCallerScope();
            } else if (strcmp(workspace, "global") == 0) {
                scope = context->getGlobalScope();
            }
            if (scope != nullptr) {
                ret = scope->insertVariable(varname, Nelson::MxArrayToArrayOf(pm)) == true ? 0 : 1;
            }
        }
    }
    return ret;
}
//=============================================================================
