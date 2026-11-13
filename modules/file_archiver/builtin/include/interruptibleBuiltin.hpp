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
#include "ArchiveInterruption.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson::FileArchiverGateway {
//=============================================================================
// Runs a long archive builtin so that Ctrl+C stops it: partial files are removed and
// empty outputs are returned while the interpreter handles the interruption.
template <ArrayOfVector (*Builtin)(int, const ArrayOfVector&)>
ArrayOfVector
interruptible(int nLhs, const ArrayOfVector& argIn)
{
    ArchiveInterruption::Scope scope(
        []() { return NelsonConfiguration::getInstance()->getInterruptPending(); });
    try {
        return Builtin(nLhs, argIn);
    } catch (const ArchiveInterruption::Interrupted&) {
        ArrayOfVector empty;
        for (int k = 0; k < nLhs; ++k) {
            empty << ArrayOf::empty();
        }
        return empty;
    }
}
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
