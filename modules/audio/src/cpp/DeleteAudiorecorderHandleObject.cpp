//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteAudiorecorderHandleObject.hpp"
#include "AudiorecorderObject.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteAudiorecorderHandleObject(const ArrayOf& A)
{
    return DeleteHandleObjects<AudiorecorderObject>(A, NLS_HANDLE_AUDIORECORDER_CATEGORY_STR,
        _W("audiorecorder handle expected."), _W("audiorecorder valid handle expected."));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
