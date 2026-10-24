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
#include "ArrayOf.hpp"
#include "HandleGenericObject.hpp"
#include "nlsAudio_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// StartFcn, StopFcn and TimerFcn of audioplayer and audiorecorder objects.
//=============================================================================
NLSAUDIO_IMPEXP bool
isValidAudioCallback(const ArrayOf& callback);
//=============================================================================
// validates and stores a callback property value (name: StartFcn, StopFcn or TimerFcn)
NLSAUDIO_IMPEXP bool
setAudioCallback(const std::wstring& name, const ArrayOf& value, bool withEventLoop,
    ArrayOf& callback, std::wstring& errorMessage);
//=============================================================================
// callable from the audio thread: the event queue is thread safe
NLSAUDIO_IMPEXP void
enqueueAudioCallback(const ArrayOf& callback, HandleGenericObject* source);
//=============================================================================
NLSAUDIO_IMPEXP std::wstring
audioCallbackToWideString(const ArrayOf& callback);
//=============================================================================
} // namespace Nelson
//=============================================================================
