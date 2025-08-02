//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MxVariables.h"
#include "MxCall.h"
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "MxArrayOf.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
static mxArray*
mexGetVariableCommon(const char* workspace, const char* varname, bool interleavedComplex)
{
    Nelson::Evaluator* mainEvaluator
        = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
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
    Nelson::Evaluator* mainEvaluator
        = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
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
