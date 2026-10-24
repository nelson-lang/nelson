//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayer_getBuiltin.hpp"
#ifdef NELSON_AUDIO_FILE_ONLY_BACKEND
#include "PortableAudioplayerObject.hpp"
#else
#include "AudioplayerObject.hpp"
#endif
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.asWideString();
    ArrayOfVector retval(1);
    if (param1.handleCategory() != NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR) {
        Error(L"Nelson:audioplayer:invalidHandle", L"audioplayer handle expected.");
    }
    auto* objPlayer =
#ifdef NELSON_AUDIO_FILE_ONLY_BACKEND
        static_cast<PortableAudioplayerObject*>(param1.asHandleScalar());
#else
        static_cast<AudioplayerObject*>(param1.asHandleScalar());
#endif
    ArrayOf res;
    if (!objPlayer->get(propertyName, res)) {
        Error(L"Nelson:audioplayer:invalidProperty", L"Invalid property: %s.", propertyName);
    }
    retval << res;
    return retval;
}
//=============================================================================
