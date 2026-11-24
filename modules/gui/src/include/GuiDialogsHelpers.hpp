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
#include <string>
#include "ArrayOf.hpp"
#include "FunctionDef.hpp"
//=============================================================================
namespace Nelson::GuiDialogHelpers {
//=============================================================================
wstringVector
asTextList(const ArrayOf& value);
//=============================================================================
std::wstring
asText(const ArrayOf& value);
//=============================================================================
std::wstring
argText(const ArrayOfVector& args, size_t index, const std::wstring& defaultValue = L"");
//=============================================================================
bool
isName(const ArrayOf& value, const std::wstring& expected);
//=============================================================================
double
numericElement(const ArrayOf& value, indexType index, double defaultValue);
//=============================================================================
FunctionDef*
findFunction(const std::string& name);
//=============================================================================
} // namespace Nelson::GuiDialogHelpers
//=============================================================================
