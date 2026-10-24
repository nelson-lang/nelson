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
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Position-free rendering of what the parser builds from source: two sources
// with the same fingerprint have the same meaning. The formatter compares the
// fingerprints of its input and output and refuses to produce a text that
// would change what the code does.
std::string
formattingSemanticFingerprint(const std::string& source);
//=============================================================================
} // namespace Nelson
//=============================================================================
