//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "ComEngine.hpp"
#include "actxserverBuiltin.hpp"
#include "COM_dispBuiltin.hpp"
#include "COM_fieldnamesBuiltin.hpp"
#include "COM_methodsBuiltin.hpp"
#include "COM_usedBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"com_engine";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "actxserver", Nelson::ComEngineGateway::actxserverBuiltin, 1, -2 },
	{ "COM_disp", Nelson::ComEngineGateway::COM_dispBuiltin, 0, 1 },
	{ "COM_fieldnames", Nelson::ComEngineGateway::COM_fieldnamesBuiltin, 1, 1 },
	{ "COM_used", Nelson::ComEngineGateway::COM_usedBuiltin, 1, 1 },

};
//=============================================================================
static bool initializeComModule(Nelson::Evaluator* eval)
{
	ComEngine::getInstance()->create();;
	return true;
}
//=============================================================================
static bool finishComModule(Nelson::Evaluator* eval)
{
	ComEngine::getInstance()->finish();
	return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeComModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishComModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
