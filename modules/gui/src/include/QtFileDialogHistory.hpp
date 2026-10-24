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
#include <QtCore/QString>
#include <string>
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGUI_IMPEXP QString
qtDialogInitialDirectory(
    const std::wstring& key, const QString& explicitPath = {}, const QString& fallbackPath = {});
//=============================================================================
NLSGUI_IMPEXP QString
qtDialogInitialSavePath(const std::wstring& key, const QString& explicitPath,
    const QString& suggestedName = {}, const QString& fallbackPath = {});
//=============================================================================
NLSGUI_IMPEXP void
qtRecordDialogSelection(const std::wstring& key, const QString& mode, const QString& selectedPath);
//=============================================================================
} // namespace Nelson
//=============================================================================
