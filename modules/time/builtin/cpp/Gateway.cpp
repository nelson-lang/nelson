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
#include "ticBuiltin.hpp"
#include "tocBuiltin.hpp"
#include "sleepBuiltin.hpp"
#include "calendarBuiltin.hpp"
#include "nowBuiltin.hpp"
#include "clockBuiltin.hpp"
#include "cputimeBuiltin.hpp"
#include "datenumBuiltin.hpp"
#include "datevecBuiltin.hpp"
#include "NelsonGateway.hpp"
#include "Evaluator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"time";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "tic", Nelson::TimeGateway::ticBuiltin, 1, 0 },
    { "toc", Nelson::TimeGateway::tocBuiltin, 1, 1 },
    { "calendar", Nelson::TimeGateway::calendarBuiltin, 1, 1 },
    { "sleep", Nelson::TimeGateway::sleepBuiltin, 1, 1 },
    { "now", Nelson::TimeGateway::nowBuiltin, 1, 0 },
    { "clock", Nelson::TimeGateway::clockBuiltin, 1, 0 },
    { "cputime", Nelson::TimeGateway::cputimeBuiltin, 1, 1 },
    { "datenum", Nelson::TimeGateway::datenumBuiltin, 1, 6 },
    { "datevec", Nelson::TimeGateway::datevecBuiltin, 6, 1 },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
