//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "AudioHelpers.hpp"
#include "NelsonGateway.hpp"
#include "audiodevinfoBuiltin.hpp"
#include "audioinfoBuiltin.hpp"
#include "audiometadataBuiltin.hpp"
#include "audioplayerBuiltin.hpp"
#include "audioplayer_deleteBuiltin.hpp"
#include "audioplayer_dispBuiltin.hpp"
#include "audioplayer_fieldnamesBuiltin.hpp"
#include "audioplayer_getBuiltin.hpp"
#include "audioplayer_ismethodBuiltin.hpp"
#include "audioplayer_ispropBuiltin.hpp"
#include "audioplayer_isvalidBuiltin.hpp"
#include "audioplayer_pauseBuiltin.hpp"
#include "audioplayer_playBuiltin.hpp"
#include "audioplayer_playblockingBuiltin.hpp"
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
    { "audiodevinfo", Nelson::AudioGateway::audiodevinfoBuiltin, 1, -1 },
    { "audioplayer", Nelson::AudioGateway::audioplayerBuiltin, 1, -1 },
    { "audioplayer_disp", Nelson::AudioGateway::audioplayer_dispBuiltin, 0, 1 },
    { "audioplayer_delete", Nelson::AudioGateway::audioplayer_deleteBuiltin, 0, 1 },
    { "audioplayer_isvalid", Nelson::AudioGateway::audioplayer_isvalidBuiltin, 1, 1 },
    { "audioplayer_fieldnames", Nelson::AudioGateway::audioplayer_fieldnamesBuiltin, 1, 1 },
    { "audioplayer_properties", Nelson::AudioGateway::audioplayer_propertiesBuiltin, 1, 1 },
    { "audioplayer_get", Nelson::AudioGateway::audioplayer_getBuiltin, 1, 2 },
    { "audioplayer_set", Nelson::AudioGateway::audioplayer_setBuiltin, 0, 3 },
    { "audioplayer_play", Nelson::AudioGateway::audioplayer_playBuiltin, 0, -1 },
    { "audioplayer_playblocking", Nelson::AudioGateway::audioplayer_playblockingBuiltin, 0, -1 },
    { "audioplayer_pause", Nelson::AudioGateway::audioplayer_pauseBuiltin, 0, 1 },
    { "audioplayer_resume", Nelson::AudioGateway::audioplayer_resumeBuiltin, 0, 1 },
    { "audioplayer_stop", Nelson::AudioGateway::audioplayer_stopBuiltin, 0, 1 },
    { "audioplayer_used", Nelson::AudioGateway::audioplayer_usedBuiltin, 1, 0 },
    { "audioplayer_isprop", Nelson::AudioGateway::audioplayer_ispropBuiltin, 1, 2 },
    { "audioplayer_ismethod", Nelson::AudioGateway::audioplayer_ismethodBuiltin, 1, 2 },
    { "playblocking", Nelson::AudioGateway::playblockingBuiltin, 0, -1 },
    { "play", Nelson::AudioGateway::playBuiltin, 0, -1 },
    { "stop", Nelson::AudioGateway::stopBuiltin, 0, 1 },
    { "resume", Nelson::AudioGateway::resumeBuiltin, 0, 1 },
    { "beep", Nelson::AudioGateway::beepBuiltin, 1, 1 },
    { "audioread", Nelson::AudioGateway::audioreadBuiltin, 2, 3 },
    { "audioinfo", Nelson::AudioGateway::audioinfoBuiltin, 1, 1 },
    { "audiometadata", Nelson::AudioGateway::audiometadataBuiltin, 1, -1 },
    { "audiosupportedformats", Nelson::AudioGateway::audiosupportedformatsBuiltin, 1, 0 },
    { "audiowrite", Nelson::AudioGateway::audiowriteBuiltin, 0, -3 },
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
