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
namespace Nelson::TimeGateway {
//=============================================================================
ArrayOfVector
timerBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_deleteBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_dispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_getBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_ismethodBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_ispropBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_isvalidBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_methodsBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_setBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_startBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_startatBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_stopBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timer_waitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timerfindBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
timerfindallBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
startBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
startatBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
stopBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
waitBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::TimeGateway
//=============================================================================
