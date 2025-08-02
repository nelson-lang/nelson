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
#include "nlsWebtools_exports.h"
#include "ArrayOf.hpp"
#include "WebOptions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSWEBTOOLS_IMPEXP std::wstring
WebREST(const std::wstring& url, const std::wstring& data, std::wstring& filename,
    const stringVector& names, const ArrayOfVector& values, WebOptions& options,
    bool haveEventsLoop, size_t evaluatorID);
//=============================================================================
};
//=============================================================================
