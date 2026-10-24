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
#include <memory>
#include <string>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Names are kept as strings so lookup, display and property access do not
// need to own parser structures.
struct ClassdefLayout
{
    std::string className;
    stringVector propertyNames;
    stringVector storagePropertyNames;
    stringVector directStoredPropertyNames;
    stringVector propertyFlags;
    stringVector inheritanceOrder;
    stringVector events;
    stringVector enumerations;
    stringVector methodNames;
};
//=============================================================================
using ClassdefLayoutPtr = std::shared_ptr<const ClassdefLayout>;
//=============================================================================
} // namespace Nelson
//=============================================================================
