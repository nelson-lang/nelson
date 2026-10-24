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
// Native classdef instance construction. isHandle selects the handle vs value
// form; argIn is [className, optional dims].
NLSINTERPRETER_IMPEXP ArrayOf
createClassdefInstance(bool isHandle, const ArrayOfVector& argIn);
//=============================================================================
// Native per-class stored-default registry.
// argIn = [className, propertyName] (retrieve -> [found, value]) or
// [className, propertyName, value] (store -> no output).
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefPropertyDefault(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// Native raw stored-property write used in constructors.
// argIn = [obj, propertyName, value].
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefSetStored(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// Value-class counterpart of evaluateClassdefSetStored: writes the property of a
// scalar value object directly, bypassing any set method, and returns the
// modified object. Used to apply property defaults in constructors.
// argIn = [obj, propertyName, value].
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefValueSetStored(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// Native handle-support operations: raw element subset, object to struct, and
// delete (invalidates each classdef handle element).
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefHandleElement(const ArrayOfVector& argIn);
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefHandleStruct(const ArrayOfVector& argIn);
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefHandleDelete(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// Native isvalid (per-element handle validity) and Copyable copy.
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefHandleIsValid(const ArrayOfVector& argIn);
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefCopy(Evaluator* eval, const ArrayOfVector& argIn);
//=============================================================================
// Native invalid-handle construction.
// argIn = [className, optional dims...]. Returns an invalid handle array.
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefInvalidHandle(const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
