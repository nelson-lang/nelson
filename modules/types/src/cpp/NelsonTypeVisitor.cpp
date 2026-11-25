//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <stdexcept>
#include "NelsonTypeVisitor.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson::TypeTraits {
//=============================================================================
void
raiseUnsupportedVisitedType()
{
    RaiseError(L"Unsupported data type.", L"Nelson:types:typeNotManaged");
    throw std::runtime_error("unreachable"); // RaiseError always throws first.
}
//=============================================================================
} // namespace Nelson::TypeTraits
//=============================================================================
