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
#include "NelsonGateway.hpp"
#include "audiodevinfoBuiltin.hpp"
#include "beepBuiltin.hpp"
#include "AudioHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"audio";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "audiodevinfo", Nelson::AudioGateway::audiodevinfoBuiltin, 1, -1 },
    { "beep", Nelson::AudioGateway::beepBuiltin, 1, 1 },
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
