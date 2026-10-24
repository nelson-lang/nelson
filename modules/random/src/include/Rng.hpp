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
#include "nlsRandom_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSRANDOM_IMPEXP std::wstring
RngGetType();
//=============================================================================
NLSRANDOM_IMPEXP void
RngSetSeed(double seed);
//=============================================================================
NLSRANDOM_IMPEXP ArrayOf
RngGetSeed();
//=============================================================================
NLSRANDOM_IMPEXP ArrayOf
RngGetState();
//=============================================================================
NLSRANDOM_IMPEXP void
RngSetDefault();
//=============================================================================
NLSRANDOM_IMPEXP void
RngShuffle();
//=============================================================================
NLSRANDOM_IMPEXP bool
RngSetEngine(double seed, const std::wstring& engineName);
//=============================================================================
NLSRANDOM_IMPEXP void
RngDelete();
//=============================================================================
NLSRANDOM_IMPEXP bool
RngSetState(const ArrayOf& st);
//=============================================================================
// Settings struct (Type, Seed, State) of the current engine, as returned by rng.
NLSRANDOM_IMPEXP ArrayOf
RngGetSettings();
//=============================================================================
// Replaces the current engine by one built from a settings struct.
// An empty State keeps the state computed from the Seed.
NLSRANDOM_IMPEXP void
RngSetSettings(const ArrayOf& settings);
//=============================================================================
// Installs a private engine built from a settings struct as the current engine
// for the lifetime of the scope. The engine that was current is parked, never
// read nor modified, and reinstated on exit (errors included).
class NLSRANDOM_IMPEXP RngEngineScope
{
public:
    explicit RngEngineScope(const ArrayOf& settings);
    ~RngEngineScope();
    RngEngineScope(const RngEngineScope&) = delete;
    RngEngineScope&
    operator=(const RngEngineScope&)
        = delete;

private:
    void* previousEngine;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
