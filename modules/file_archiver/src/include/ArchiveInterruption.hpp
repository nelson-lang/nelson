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
#include <functional>
#include <stdexcept>
#include "nlsFile_archiver_exports.h"
//=============================================================================
// Lets long archive operations stop on Ctrl+C: the builtins install a check for the
// calling thread, the loops call throwIfInterrupted().
namespace Nelson::ArchiveInterruption {
//=============================================================================
class Interrupted : public std::runtime_error
{
public:
    Interrupted() : std::runtime_error("Archive operation interrupted.") { }
};
//=============================================================================
// Installs check for the current thread until the scope ends.
class NLSFILE_ARCHIVER_IMPEXP Scope
{
public:
    explicit Scope(std::function<bool()> check);
    ~Scope();
    Scope(const Scope&) = delete;
    Scope&
    operator=(const Scope&) = delete;

private:
    std::function<bool()> previous;
};
//=============================================================================
// Throws Interrupted when the check of the current thread says so.
NLSFILE_ARCHIVER_IMPEXP void
throwIfInterrupted();
//=============================================================================
} // namespace Nelson::ArchiveInterruption
//=============================================================================
