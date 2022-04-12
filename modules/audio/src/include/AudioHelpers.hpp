//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsAudio_exports.h"
//=============================================================================
namespace Nelson {
NLSAUDIO_IMPEXP bool
initializeAudio();
NLSAUDIO_IMPEXP bool
terminateAudio();
NLSAUDIO_IMPEXP ArrayOf
audioDevInfo();
} // namespace Nelson
//=============================================================================
