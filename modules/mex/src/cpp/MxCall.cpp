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
#include "mex.h"
#include "MxCall.h"
#include "Evaluator.hpp"
#include "MxArrayOf.hpp"
//=============================================================================
static Nelson::Evaluator* mainEvaluator = nullptr;
//=============================================================================
void
mexSetEvaluator(void* eval)
{
    mainEvaluator = (Nelson::Evaluator*)eval;
}
//=============================================================================
static int
mexCallNELSON(int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    Nelson::ArrayOfVector argIn;
    Nelson::ArrayOfVector argOut;
    for (int r = 0; r < nrhs; ++r) {
        argIn.push_back(Nelson::MxArrayToArrayOf(prhs[r]));
    }
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
                            plhs[i] = Nelson::ArrayOfToMxArray(argOut[i]);
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
mexCallMATLAB(int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    return mexCallNELSON(nlhs, plhs, nrhs, prhs, functionName);
}
//=============================================================================
static Nelson::ArrayOf
createMexception(Nelson::Exception& e)
{
    Nelson::wstringVector fieldnames;
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
    Nelson::ArrayOf res = Nelson::ArrayOf::structConstructor(fieldnames, fieldvalues);

    res.setStructType("mexception");
    return res;
}
//=============================================================================
static mxArray*
mexCallNELSONWithTrap(
    int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    Nelson::ArrayOfVector argIn;
    Nelson::ArrayOfVector argOut;
    for (int r = 0; r < nrhs; ++r) {
        argIn.push_back(Nelson::MxArrayToArrayOf(prhs[r]));
    }
    if (mainEvaluator != nullptr) {
        Nelson::Context* context = mainEvaluator->getContext();
        if (context != nullptr) {
            Nelson::FunctionDef* funcDef = nullptr;
            if (context->lookupFunction(functionName, funcDef)) {
                try {
                    argOut = funcDef->evaluateFunction(mainEvaluator, argIn, nlhs);
                } catch (Nelson::Exception& e) {
                    return Nelson::ArrayOfToMxArray(createMexception(e));
                }
                for (int i = 0; i < nlhs; i++) {
                    plhs[i] = Nelson::ArrayOfToMxArray(argOut[i]);
                }
                return nullptr;
            }
        }
    }
    Nelson::Exception e("No evaluator.");
    return Nelson::ArrayOfToMxArray(createMexception(e));
}
//=============================================================================
NLSMEX_IMPEXP
mxArray*
mexCallMATLABWithTrap(
    int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName)
{
    return mexCallNELSONWithTrap(nlhs, plhs, nrhs, prhs, functionName);
}
//=============================================================================
int
mexEvalString(const char* command)
{
    if (mainEvaluator != nullptr) {
        if (mainEvaluator->evaluateString(command, true)) {
            return 0;
        }
    }
    return 1;
}
//=============================================================================
mxArray*
mexEvalStringWithTrap(const char* command)
{
    if (mainEvaluator != nullptr) {
        try {
            bool res = mainEvaluator->evaluateString(command, true);
        } catch (Nelson::Exception& e) {
            return Nelson::ArrayOfToMxArray(createMexception(e));
        }
    }
    return nullptr;
}
//=============================================================================
