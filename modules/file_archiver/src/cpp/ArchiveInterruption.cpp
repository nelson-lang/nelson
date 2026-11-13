//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <utility>
#include "ArchiveInterruption.hpp"
//=============================================================================
namespace Nelson::ArchiveInterruption {
//=============================================================================
static thread_local std::function<bool()> currentCheck;
//=============================================================================
Scope::Scope(std::function<bool()> check) : previous(std::move(currentCheck))
{
    currentCheck = std::move(check);
}
//=============================================================================
Scope::~Scope() { currentCheck = std::move(previous); }
//=============================================================================
void
throwIfInterrupted()
{
    if (currentCheck && currentCheck()) {
        throw Interrupted();
    }
}
//=============================================================================
} // namespace Nelson::ArchiveInterruption
//=============================================================================
