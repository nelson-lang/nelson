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
#include "nlsGui_exports.h"
#include <QtCore/QString>
#include <string>
//=============================================================================
namespace Nelson {
NLSGUI_IMPEXP std::wstring
QStringTowstring(QString qstr);
NLSGUI_IMPEXP QString
wstringToQString(const std::wstring& wstr);
} // namespace Nelson
//=============================================================================
