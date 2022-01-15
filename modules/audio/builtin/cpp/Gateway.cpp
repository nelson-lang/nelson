//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "AudioHelpers.hpp"
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
    { "audiodevinfo", (void*)Nelson::AudioGateway::audiodevinfoBuiltin, 1, -1 },
    { "audioplayer", (void*)Nelson::AudioGateway::audioplayerBuiltin, 1, -1 },
    { "audioplayer_display", (void*)Nelson::AudioGateway::audioplayer_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "audioplayer_disp", (void*)Nelson::AudioGateway::audioplayer_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "audioplayer_delete", (void*)Nelson::AudioGateway::audioplayer_deleteBuiltin, 0, 1 },
    { "audioplayer_isvalid", (void*)Nelson::AudioGateway::audioplayer_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "audioplayer_fieldnames", (void*)Nelson::AudioGateway::audioplayer_fieldnamesBuiltin, 1, 1 },
    { "audioplayer_properties", (void*)Nelson::AudioGateway::audioplayer_propertiesBuiltin, 1, 1 },
    { "audioplayer_get", (void*)Nelson::AudioGateway::audioplayer_getBuiltin, 1, 2 },
    { "audioplayer_set", (void*)Nelson::AudioGateway::audioplayer_setBuiltin, 0, 3 },
    { "audioplayer_play", (void*)Nelson::AudioGateway::audioplayer_playBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "audioplayer_playblocking", (void*)Nelson::AudioGateway::audioplayer_playblockingBuiltin, 0,
        -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "audioplayer_pause", (void*)Nelson::AudioGateway::audioplayer_pauseBuiltin, 0, 1 },
    { "audioplayer_resume", (void*)Nelson::AudioGateway::audioplayer_resumeBuiltin, 0, 1 },
    { "audioplayer_stop", (void*)Nelson::AudioGateway::audioplayer_stopBuiltin, 0, 1 },
    { "audioplayer_used", (void*)Nelson::AudioGateway::audioplayer_usedBuiltin, 1, 0 },
    { "audioplayer_isprop", (void*)Nelson::AudioGateway::audioplayer_ispropBuiltin, 1, 2 },
    { "audioplayer_ismethod", (void*)Nelson::AudioGateway::audioplayer_ismethodBuiltin, 1, 2 },
    { "playblocking", (void*)Nelson::AudioGateway::playblockingBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "play", (void*)Nelson::AudioGateway::playBuiltin, 0, -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "stop", (void*)Nelson::AudioGateway::stopBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "resume", (void*)Nelson::AudioGateway::resumeBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "beep", (void*)Nelson::AudioGateway::beepBuiltin, 1, 1 },
    { "audioread", (void*)Nelson::AudioGateway::audioreadBuiltin, 2, 3 },
    { "audioinfo", (void*)Nelson::AudioGateway::audioinfoBuiltin, 1, 1 },
    { "audiometadata", (void*)Nelson::AudioGateway::audiometadataBuiltin, 1, -1 },
    { "audiosupportedformats", (void*)Nelson::AudioGateway::audiosupportedformatsBuiltin, 1, 0 },
    { "audiowrite", (void*)Nelson::AudioGateway::audiowriteBuiltin, 0, -3 },
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
