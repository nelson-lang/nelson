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
namespace Nelson::ErrorManagerGateway {
//=============================================================================
ArrayOfVector
MExceptionBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_lastBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_addCauseBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_addCorrectionBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_getReportBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_eqBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_neBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_isequalBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_methodsBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
MException_subsasgnBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
