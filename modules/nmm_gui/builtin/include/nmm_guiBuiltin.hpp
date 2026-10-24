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
//=============================================================================
namespace Nelson {
class Evaluator;
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace Nelson::NmmGuiGateway {
//=============================================================================
// nmm_gui(): open (or focus) the singleton package manager window.
// Needs the Evaluator so RPC calls can be marshalled onto the interpreter.
ArrayOfVector
nmm_guiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// __nmm_gui_notify__(channel): push a refresh signal to the open window
// (called at the end of a queued install/remove command).
ArrayOfVector
nmm_gui_notifyBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// __nmm_gui_i18n__([locale]): embedded window string catalogs.
ArrayOfVector
nmm_gui_i18nBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// __nmm_gui_updates__([count]): update count shown on the desktop badges.
ArrayOfVector
nmm_gui_updatesBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// __nmm_gui_prefs__([jsonText]): stored window preferences.
ArrayOfVector
nmm_gui_prefsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
void
closeNmmGuiWindow();
//=============================================================================
} // namespace Nelson::NmmGuiGateway
//=============================================================================
