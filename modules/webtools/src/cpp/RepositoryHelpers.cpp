//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <git2.h>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
gitErrorCodeToMessage(int errorCode)
{
    std::wstring errorMessage;
    if (errorCode != 0) {
        const git_error* e = giterr_last();
        if (e) {
            std::wstring msg = utf8_to_wstring(e->message);
            errorMessage
                = fmt::sprintf(_W("repository error %d/%d: %ls"), errorCode, e->klass, msg);
        } else {
            errorMessage = fmt::sprintf(_W("repository error %d"), errorCode);
        }
    }
    return errorMessage;
}
//=============================================================================
};
//=============================================================================
