//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteAudioplayerHandleObject.hpp"
#include "AudioplayerObject.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteAudioplayerHandleObject(const ArrayOf& A)
{
    return DeleteHandleObjects<AudioplayerObject>(A, NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR,
        _W("audioplayer handle expected."), _W("audioplayer valid handle expected."));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
