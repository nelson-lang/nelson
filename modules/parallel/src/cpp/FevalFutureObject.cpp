//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalFutureObject.hpp"
//=============================================================================
FevalFutureObject::FevalFutureObject(const std::wstring& functionName)
    : FutureObject(functionName)
    , HandleGenericObject(NLS_HANDLE_FEVALFUTURE_CATEGORY_STR, this, false)
{
    setType(NLS_HANDLE_FEVALFUTURE_CATEGORY_STR);
}
//=============================================================================
