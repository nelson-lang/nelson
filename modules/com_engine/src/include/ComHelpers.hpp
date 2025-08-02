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
#define WIN32_LEAN_AND_MEAN
#include <Ole2.h>
#include <Windows.h>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isMethodCom(IDispatch* pDisp, std::wstring methodToSearch);
bool
isPropertyGetCom(IDispatch* pDisp, std::wstring propertyToSearch);
bool
isPropertyPutCom(IDispatch* pDisp, std::wstring propertyToSearch);
//=============================================================================
} // namespace Nelson
//=============================================================================
