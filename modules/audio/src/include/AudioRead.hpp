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
NLSAUDIO_IMPEXP ArrayOfVector
AudioRead(const std::wstring& filename, double dstart, double dend, const std::wstring& datatype,
    std::wstring& errorMessage);
}
//=============================================================================
