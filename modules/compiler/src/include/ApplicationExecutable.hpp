//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include "nlsCompiler_exports.h"
#include <array>
#include <cstdint>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
NLSCOMPILER_IMPEXP void
wrapApplicationInstaller(const std::wstring& launcher, const std::wstring& installer,
    const std::wstring& destination, bool runtime = false);
void
validateApplicationResourceTarget(const std::wstring& filename);
NLSCOMPILER_IMPEXP void
setApplicationIcon(const std::wstring& filename, const std::vector<std::wstring>& images);
NLSCOMPILER_IMPEXP std::string
applicationLauncherContract(const std::string& filename);
NLSCOMPILER_IMPEXP void
validateApplicationRuntimeLog(const std::string& filename);
NLSCOMPILER_IMPEXP void
setApplicationRuntimeLog(const std::string& filename, const std::string& logFilename);
NLSCOMPILER_IMPEXP std::array<uint16_t, 4>
parseApplicationVersion(const std::wstring& version);
NLSCOMPILER_IMPEXP void
setApplicationVersion(const std::wstring& filename, const std::wstring& version);
NLSCOMPILER_IMPEXP void
publishApplicationFiles(const std::wstring& source, const std::wstring& destination);
NLSCOMPILER_IMPEXP void
recoverApplicationFiles(const std::wstring& destination);
NLSCOMPILER_IMPEXP void
replaceApplicationFiles(
    const std::wstring& source, const std::wstring& destination, const std::wstring& expected);
//=============================================================================
} // namespace Nelson
//=============================================================================
