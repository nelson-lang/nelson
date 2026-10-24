//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
// Per-element callback evaluation shared by cellfun and arrayfun.
//=============================================================================
#include "ArrayOf.hpp"
#include "BytecodeVM.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
//=============================================================================
namespace Nelson::DataStructuresGateway {
//=============================================================================
// Error raised when a function is asked for an output it does not provide.
inline bool
isTooManyOutputsError(const NelsonException& e)
{
    return e.identifier() == L"Nelson:tooManyOutputs"
        || e.identifier() == L"Nelson:nargoutchk:tooManyOutputs";
}
//=============================================================================
// Error information handed to the error handler: the identifier and message
// of the error and the (1-based) linear index of the element being processed.
inline ArrayOf
errorHandlerInfo(const NelsonException& e, indexType elementIndex)
{
    stringVector fieldnames = { "identifier", "message", "index" };
    ArrayOfVector values;
    values.push_back(ArrayOf::characterArray(e.identifier()));
    values.push_back(ArrayOf::characterArray(e.message()));
    values.push_back(ArrayOf::doubleScalar(static_cast<double>(elementIndex)));
    return ArrayOf::structScalar(fieldnames, values);
}
//=============================================================================
// Evaluates the callback on one element. With an error handler, an error of
// the callback is forwarded to the handler, except a "too many outputs" error
// when `rethrowTooManyOutputs` is set (the caller then retries with nargout 0).
inline ArrayOfVector
evaluateCallback(Evaluator* eval, FunctionDefPtr fptr, FunctionDefPtr fptrHandleError,
    const BytecodeChunk* directChunk, const ArrayOfVector& input, int nargout,
    bool rethrowTooManyOutputs, indexType elementIndex)
{
    if (fptrHandleError == nullptr) {
        return directChunk != nullptr
            ? BytecodeVM::executeFunction(eval, *directChunk, input, nargout, false)
            : fptr->evaluateFunction(eval, input, nargout);
    }
    try {
        return fptr->evaluateFunction(eval, input, nargout);
    } catch (NelsonException& e) {
        if (rethrowTooManyOutputs && isTooManyOutputsError(e)) {
            throw;
        }
        ArrayOfVector in2;
        in2.push_back(errorHandlerInfo(e, elementIndex));
        in2 += input;
        return fptrHandleError->evaluateFunction(eval, in2, nargout);
    }
}
//=============================================================================
} // namespace Nelson::DataStructuresGateway
//=============================================================================
