//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AudioHelpers.hpp"
#include "OverloadName.hpp"
#include "NelsonGateway.hpp"
#include "audiodevinfoBuiltin.hpp"
#include "audioinfoBuiltin.hpp"
#include "audiometadataBuiltin.hpp"
#include "audioplayerBuiltin.hpp"
#include "audioplayer_deleteBuiltin.hpp"
#include "audioplayer_displayBuiltin.hpp"
#include "audioplayer_fieldnamesBuiltin.hpp"
#include "audioplayer_getBuiltin.hpp"
#include "audioplayer_ismethodBuiltin.hpp"
#include "audioplayer_ispropBuiltin.hpp"
#include "audioplayer_isvalidBuiltin.hpp"
#include "audioplayer_pauseBuiltin.hpp"
#include "audioplayer_playBuiltin.hpp"
#include "audioplayer_propertiesBuiltin.hpp"
#include "audioplayer_resumeBuiltin.hpp"
#include "audioplayer_setBuiltin.hpp"
#include "audioplayer_stopBuiltin.hpp"
#include "audioplayer_usedBuiltin.hpp"
#include "audiorecorderBuiltin.hpp"
#include "audiorecorder_deleteBuiltin.hpp"
#include "audiorecorder_displayBuiltin.hpp"
#include "audiorecorder_dispBuiltin.hpp"
#include "audiorecorder_fieldnamesBuiltin.hpp"
#include "audiorecorder_getBuiltin.hpp"
#include "audiorecorder_ismethodBuiltin.hpp"
#include "audiorecorder_ispropBuiltin.hpp"
#include "audiorecorder_isvalidBuiltin.hpp"
#include "audiorecorder_pauseBuiltin.hpp"
#include "audiorecorder_playBuiltin.hpp"
#include "audiorecorder_propertiesBuiltin.hpp"
#include "audiorecorder_recordblockingBuiltin.hpp"
#include "audiorecorder_recordBuiltin.hpp"
#include "audiorecorder_resumeBuiltin.hpp"
#include "audiorecorder_setBuiltin.hpp"
#include "audiorecorder_startBuiltin.hpp"
#include "audiorecorder_stopBuiltin.hpp"
#include "audiorecorder_usedBuiltin.hpp"
#include "audiorecorder_invokeBuiltin.hpp"
#include "audioreadBuiltin.hpp"
#include "audiosupportedformatsBuiltin.hpp"
#include "audiowriteBuiltin.hpp"
#include "beepBuiltin.hpp"
#include "playBuiltin.hpp"
#include "playblockingBuiltin.hpp"
#include "resumeBuiltin.hpp"
#include "stopBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"audio";
//=============================================================================
static const nlsGateway gateway[] = {
    //=============================================================================
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_dispBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_deleteBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_isvalidBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "properties"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_propertiesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "set"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_setBuiltin, 0, 3, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "play"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_playBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "playblocking"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_playblockingBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "pause"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_pauseBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "resume"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_resumeBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "stop"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_stopBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_ispropBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, "ismethod"),
        (ptrBuiltin)Nelson::AudioGateway::audioplayer_ismethodBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    // audiorecorder overloads
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::AudioGateway::audiorecorder_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::AudioGateway::audiorecorder_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::AudioGateway::audiorecorder_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "set"),
        (ptrBuiltin)Nelson::AudioGateway::audiorecorder_setBuiltin, 0, 3, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },

    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "delete"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_deleteBuiltin, 0, 1, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "isvalid"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_isvalidBuiltin, 1, 1, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "fieldnames"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "properties"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_propertiesBuiltin, 1, 1, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "start"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_startBuiltin, 0, -1,
    //    CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "pause"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_pauseBuiltin, 0, 1, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "resume"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_resumeBuiltin, 0, 1, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "stop"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_stopBuiltin, 0, 1, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    ////{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "play"),
    ////    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_playBuiltin, 0, -1,
    ////    CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "isprop"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_ispropBuiltin, 1, 2, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },
    //{ OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "ismethod"),
    //    (ptrBuiltin)Nelson::AudioGateway::audiorecorder_ismethodBuiltin, 1, 2, CPP_BUILTIN,
    //    NLS_OVERLOAD_AUTO_OFF },

    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, "invoke"),
        (ptrBuiltin)Nelson::AudioGateway::audiorecorder_invokeBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },

    { "record", (ptrBuiltin)Nelson::AudioGateway::audiorecorder_recordBuiltin, 0, -2, CPP_BUILTIN },
    { "recordblocking", (ptrBuiltin)Nelson::AudioGateway::audiorecorder_recordblockingBuiltin, 0, 2,
        CPP_BUILTIN },
    { "audiorecorder_used", (ptrBuiltin)Nelson::AudioGateway::audiorecorder_usedBuiltin, 1, 0 },
    { "audiorecorder", (ptrBuiltin)Nelson::AudioGateway::audiorecorderBuiltin, 1, -1 },
    //=============================================================================
    { "audioplayer_used", (ptrBuiltin)Nelson::AudioGateway::audioplayer_usedBuiltin, 1, 0 },
    { "audiodevinfo", (ptrBuiltin)Nelson::AudioGateway::audiodevinfoBuiltin, 1, -1 },
    { "audioplayer", (ptrBuiltin)Nelson::AudioGateway::audioplayerBuiltin, 1, -1 },
    { "playblocking", (ptrBuiltin)Nelson::AudioGateway::playblockingBuiltin, 0, -1 },
    { "play", (ptrBuiltin)Nelson::AudioGateway::playBuiltin, 0, -1 },
    { "stop", (ptrBuiltin)Nelson::AudioGateway::stopBuiltin, 0, 1 },
    { "resume", (ptrBuiltin)Nelson::AudioGateway::resumeBuiltin, 0, 1 },
    { "beep", (ptrBuiltin)Nelson::AudioGateway::beepBuiltin, 1, 1 },
    { "audioread", (ptrBuiltin)Nelson::AudioGateway::audioreadBuiltin, 2, 3 },
    { "audioinfo", (ptrBuiltin)Nelson::AudioGateway::audioinfoBuiltin, 1, 1 },
    { "audiometadata", (ptrBuiltin)Nelson::AudioGateway::audiometadataBuiltin, 1, -1 },
    { "audiosupportedformats", (ptrBuiltin)Nelson::AudioGateway::audiosupportedformatsBuiltin, 1,
        0 },
    { "audiowrite", (ptrBuiltin)Nelson::AudioGateway::audiowriteBuiltin, 0, -3 },
};
//=============================================================================
static bool
initializeAudioModule(Nelson::Evaluator* eval)
{
    initializeAudio();
    return true;
}
//=============================================================================
static bool
finishAudioModule(Nelson::Evaluator* eval)
{
    terminateAudio();
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeAudioModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishAudioModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
