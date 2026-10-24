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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
class MacroFunctionDef;
NLSINTERPRETER_IMPEXP bool
isBytecodeArtifact(const std::wstring& filename);
NLSINTERPRETER_IMPEXP void
compileBytecodeArtifact(const std::wstring& source, const std::wstring& destination);
NLSINTERPRETER_IMPEXP void
loadBytecodeArtifact(MacroFunctionDef& function, const std::wstring& filename);
NLSINTERPRETER_IMPEXP std::string
inspectBytecodeArtifact(const std::wstring& filename);
NLSINTERPRETER_IMPEXP std::string
bytecodeEngineFingerprint();
NLSINTERPRETER_IMPEXP void
verifyApplicationRuntime(const std::wstring& manifest);
//=============================================================================
} // namespace Nelson
//=============================================================================
