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
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline void
OverloadRequired(const std::string& functionName)
{
    std::string msgfmt
        = _("Check for incorrect argument data type or missing argument in call to function '%s'.");
    size_t size = msgfmt.size() + functionName.size();
    std::string msg(size, '\0');
    int nChars = std::snprintf(&msg[0], size, msgfmt.c_str(), functionName.c_str());
    msg.resize(nChars);
    Error(msg, "Nelson:UndefinedFunction");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
