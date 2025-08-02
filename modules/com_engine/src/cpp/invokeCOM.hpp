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
#include <Ole2.h>
#include <Windows.h>
#include <string>
//=============================================================================
bool
invokeCom(int autoType, VARIANT* pvResult, std::wstring& errorMessage, IDispatch* pDisp,
    std::wstring propertyName, int cArgs, VARIANT* pArgs);
//=============================================================================
