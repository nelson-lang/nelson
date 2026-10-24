//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayer_deleteBuiltin.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PortableAudioplayerObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_deleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf player = argIn[0];
    if (player.isHandle()) {
        DeleteHandleObjects<PortableAudioplayerObject>(player, NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR,
            L"audioplayer handle expected.", L"audioplayer valid handle expected.");
    }
    return {};
}
//=============================================================================
