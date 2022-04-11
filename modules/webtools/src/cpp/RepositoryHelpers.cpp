//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <git2.h>
#include "characters_encoding.hpp"
#include "RepositoryHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
gitErrorCodeToMessage(int errorCode)
{
#define BUFFER_SIZE 4096
    std::wstring errorMessage;
    if (errorCode != 0) {
        const git_error* e = giterr_last();
        wchar_t buffer[BUFFER_SIZE];
        if (e) {
            std::wstring msg = utf8_to_wstring(e->message);
            swprintf(buffer, BUFFER_SIZE, _W("repository error %d/%d: %ls").c_str(), errorCode,
                e->klass, msg.c_str());
        } else {
            swprintf(buffer, BUFFER_SIZE, _W("repository error %d").c_str(), errorCode);
        }
        errorMessage = std::wstring(buffer);
    }
    return errorMessage;
}
//=============================================================================
};
//=============================================================================
