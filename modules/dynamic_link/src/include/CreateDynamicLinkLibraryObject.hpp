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
#include "DynamicLinkLibraryObject.hpp"
#include "nlsDynamic_link_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSDYNAMIC_LINK_IMPEXP ArrayOf
createDynamicLinkSymbolObject(const ArrayOf& dllibObject, const std::wstring& symbol,
    const std::wstring& returnType, const wstringVector& argumentsType);
//=============================================================================
} // namespace Nelson
//=============================================================================
