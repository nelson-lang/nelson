//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayer_stopBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PortableAudioplayerObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_stopBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 1);
    ArrayOf player = argIn[0];
    if (player.handleCategory() != NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR) {
        Error(L"Nelson:audioplayer:invalidHandle", L"audioplayer handle expected.");
    }
    auto* objPlayer = static_cast<PortableAudioplayerObject*>(player.asHandleScalar());
    objPlayer->stop();
    return {};
}
//=============================================================================
