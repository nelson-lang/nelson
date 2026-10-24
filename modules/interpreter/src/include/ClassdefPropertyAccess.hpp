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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Native classdef handle property read.
// argIn = [obj, propertyName]. Honors access policy, dynamic properties,
// dependent getters and Get property events; returns the property value.
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefHandleGet(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// Native classdef handle property write.
// argIn = [obj, propertyName, value]. Honors access policy, Constant/AbortSet,
// validation, dynamic/dependent setters and Set property events.
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefHandleSet(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// Native dynamicprops support (add and remove a dynamic property).
// argIn = [obj, propertyName].
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefAddDynamicProperty(Evaluator* eval, const ArrayOfVector& argIn);
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefRemoveDynamicProperty(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
