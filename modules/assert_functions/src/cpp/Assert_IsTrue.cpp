//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Assert_IsTrue.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
logical
Assert_IsTrue(logical value, const std::wstring& modifiedmsg, std::wstring& msg)
{
    if (value == 0) {
        if (!modifiedmsg.empty()) {
            msg = modifiedmsg;
        } else {
            msg = _W("Assertion failed: found false entry in condition = false.");
        }
    } else {
        msg.clear();
    }
    return value;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
