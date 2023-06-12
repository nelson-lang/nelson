//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::ElementaryFunctionsGateway {
ArrayOfVector
isequalCommonBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn, const std::string& functionName);
//=============================================================================
ArrayOfVector
struct_isequalCommonBuiltin(
    Evaluator* eval, const ArrayOfVector& argIn, const std::string& functionName);
//=============================================================================
ArrayOfVector
cell_isequalCommonBuiltin(
    Evaluator* eval, const ArrayOfVector& argIn, const std::string& functionName);
//=============================================================================
ArrayOfVector
char_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
logical_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
double_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
single_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
int8_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
int16_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
int32_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
int64_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
uint8_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
uint16_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
uint32_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================
ArrayOfVector
uint64_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================s
ArrayOfVector
string_isequalCommonBuiltin(const ArrayOfVector& argIn, const std::string& functionName,
    bool mustBeSameType, bool nanIsSame);
//=============================================================================s
}
