//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mex.h"
#include "MxCall.h"
#include "Evaluator.hpp"
#include "MxArrayOf.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
static int
mexCallNELSON(int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName,
    bool interleavedComplex)
{
    Nelson::ArrayOfVector argIn;
    Nelson::ArrayOfVector argOut;
    for (int r = 0; r < nrhs; ++r) {
        argIn.push_back(Nelson::MxArrayToArrayOf(prhs[r]));
    }
    Nelson::Evaluator* mainEvaluator
        = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
    if (mainEvaluator != nullptr) {
        Nelson::Context* context = mainEvaluator->getContext();
        if (context != nullptr) {
            Nelson::FunctionDef* funcDef = nullptr;
            if (context->lookupFunction(functionName, funcDef)) {
                if (funcDef != nullptr) {
                    try {
                        argOut = funcDef->evaluateFunction(mainEvaluator, argIn, nlhs);
                    } catch (Nelson::Exception&) {
                        return 1;
                    }
                    for (int i = 0; i < nlhs; i++) {
                        if (i < argOut.size()) {
                            plhs[i] = Nelson::ArrayOfToMxArray(argOut[i], interleavedComplex);
                        }
                    }
                    return 0;
                }
            }
        }
    }
    return 1;
}
//=============================================================================
int
mexCallMATLABSeparatedComplex(
    int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    return mexCallNELSON(nlhs, plhs, nrhs, prhs, functionName, false);
}
//=============================================================================
int
mexCallMATLABInterleavedComplex(
    int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    return mexCallNELSON(nlhs, plhs, nrhs, prhs, functionName, true);
}
//=============================================================================
static Nelson::ArrayOf
createMexception(Nelson::Exception& e)
{
    Nelson::wstringVector fieldnames;
    fieldnames.reserve(4);
    Nelson::ArrayOfVector fieldvalues;

    fieldnames.push_back(L"identifier");
    fieldnames.push_back(L"message");
    fieldnames.push_back(L"cause");
    fieldnames.push_back(L"stack");

    fieldvalues.push_back(Nelson::ArrayOf::characterArrayConstructor(e.getIdentifier()));
    fieldvalues.push_back(Nelson::ArrayOf::characterArrayConstructor(e.getMessage()));
    fieldvalues.push_back(Nelson::ArrayOf::emptyConstructor());
    Nelson::Dimensions emptyDims(1, 0);
    fieldvalues.push_back(
        Nelson::ArrayOf::emptyStructConstructor(Nelson::wstringVector(), emptyDims));
    std::wstring className = L"MException";
    Nelson::ArrayOf res = Nelson::ArrayOf::classConstructor(className, fieldnames, fieldvalues);
    return res;
}
//=============================================================================
static mxArray*
mexCallNELSONWithTrap(int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[],
    const char* functionName, bool interleavedComplex)
{
    Nelson::ArrayOfVector argIn;
    Nelson::ArrayOfVector argOut;
    for (int r = 0; r < nrhs; ++r) {
        argIn.push_back(Nelson::MxArrayToArrayOf(prhs[r]));
    }
    Nelson::Evaluator* mainEvaluator
        = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
    if (mainEvaluator != nullptr) {
        Nelson::Context* context = mainEvaluator->getContext();
        if (context != nullptr) {
            Nelson::FunctionDef* funcDef = nullptr;
            if (context->lookupFunction(functionName, funcDef)) {
                try {
                    argOut = funcDef->evaluateFunction(mainEvaluator, argIn, nlhs);
                } catch (Nelson::Exception& e) {
                    return Nelson::ArrayOfToMxArray(createMexception(e), interleavedComplex);
                }
                for (int i = 0; i < nlhs; i++) {
                    plhs[i] = Nelson::ArrayOfToMxArray(argOut[i], interleavedComplex);
                }
                return nullptr;
            }
        }
    }
    Nelson::Exception e("No evaluator.");
    return Nelson::ArrayOfToMxArray(createMexception(e), interleavedComplex);
}
//=============================================================================
NLSMEX_IMPEXP
mxArray*
mexCallMATLABWithTrapSeparatedComplex(
    int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    return mexCallNELSONWithTrap(nlhs, plhs, nrhs, prhs, functionName, false);
}
//=============================================================================
NLSMEX_IMPEXP
mxArray*
mexCallMATLABWithTrapInterleavedComplex(
    int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    return mexCallNELSONWithTrap(nlhs, plhs, nrhs, prhs, functionName, true);
}
//=============================================================================
int
mexEvalString(const char* command)
{
    Nelson::Evaluator* mainEvaluator
        = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
    if (mainEvaluator != nullptr) {
        if (mainEvaluator->evaluateString(command, true)) {
            return 0;
        }
    }
    return 1;
}
//=============================================================================
static mxArray*
mexEvalStringWithTrapInternal(const char* command, bool interleavedComplex)
{
    Nelson::Evaluator* mainEvaluator
        = (Nelson::Evaluator*)Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
    if (mainEvaluator != nullptr) {
        try {
            bool res = mainEvaluator->evaluateString(command, true);
        } catch (Nelson::Exception& e) {
            return Nelson::ArrayOfToMxArray(createMexception(e), interleavedComplex);
        }
    }
    return nullptr;
}
//=============================================================================
mxArray*
mexEvalStringWithTrapInterleavedComplex(const char* command)
{
    return mexEvalStringWithTrapInternal(command, true);
}
//=============================================================================
mxArray*
mexEvalStringWithTrapSeparatedComplex(const char* command)
{
    return mexEvalStringWithTrapInternal(command, false);
}
//=============================================================================
