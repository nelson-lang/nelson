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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::HandleGateway {
//=============================================================================
ArrayOfVector
classdefHandleBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefHandleGetBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefHandleSetBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefHandleIsValidBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefHandleStructBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefHandleDeleteBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefAddListenerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefNotifyBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefListenerDeleteBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
classdefListenerIsValidBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
