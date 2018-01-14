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
#include "beepBuiltin.hpp"
#include "audiodevinfoBuiltin.hpp"
#include "audioplayerBuiltin.hpp"
#include "audioplayer_setBuiltin.hpp"
#include "audioplayer_getBuiltin.hpp"
#include "audioplayer_dispBuiltin.hpp"
#include "audioplayer_deleteBuiltin.hpp"
#include "audioplayer_usedBuiltin.hpp"
#include "audioplayer_fieldnamesBuiltin.hpp"
#include "audioplayer_propertiesBuiltin.hpp"
#include "playBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"audio";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "beep", Nelson::AudioGateway::beepBuiltin, 1, 1 },
    { "audiodevinfo", Nelson::AudioGateway::audiodevinfoBuiltin, 1, -1 },
    { "audioplayer", Nelson::AudioGateway::audioplayerBuiltin, 1, -1 },
    { "audioplayer_disp", Nelson::AudioGateway::audioplayer_dispBuiltin, 0, 1 },
    { "audioplayer_fieldnames", Nelson::AudioGateway::audioplayer_fieldnamesBuiltin, 1, 1 },
    { "audioplayer_properties", Nelson::AudioGateway::audioplayer_propertiesBuiltin, 1, 1 },
    { "audioplayer_get", Nelson::AudioGateway::audioplayer_getBuiltin, 1, 2 },
    { "audioplayer_set", Nelson::AudioGateway::audioplayer_setBuiltin, 0, 3 },
    { "play", Nelson::AudioGateway::playBuiltin, 0, 1 },
};
//=============================================================================
static bool initializeAudioModule(Nelson::Evaluator* eval)
{
    initializeAudio();
    return true;
}
//=============================================================================
static bool finishAudioModule(Nelson::Evaluator* eval)
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
