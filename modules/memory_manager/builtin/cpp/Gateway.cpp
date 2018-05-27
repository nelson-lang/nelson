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
#include "acquirevarBuiltin.hpp"
#include "assigninBuiltin.hpp"
#include "clearBuiltin.hpp"
#include "globalBuiltin.hpp"
#include "isglobalBuiltin.hpp"
#include "isvarBuiltin.hpp"
#include "memoryBuiltin.hpp"
#include "persistentBuiltin.hpp"
#include "varislockBuiltin.hpp"
#include "varlockBuiltin.hpp"
#include "varunlockBuiltin.hpp"
#include "whoBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"memory_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "clear", Nelson::MemoryGateway::clearBuiltin, 0, 1 },
    { "who", Nelson::MemoryGateway::whoBuiltin, 1, 1 },
    { "global", Nelson::MemoryGateway::globalBuiltin, 0, -1 },
    { "isglobal", Nelson::MemoryGateway::isglobalBuiltin, 1, 1 },
    { "persistent", Nelson::MemoryGateway::persistentBuiltin, 0, -1 },
    { "assignin", Nelson::MemoryGateway::assigninBuiltin, 0, 3 },
    { "acquirevar", Nelson::MemoryGateway::acquirevarBuiltin, 1, 3 },
    { "varlock", Nelson::MemoryGateway::varlockBuiltin, -1, 2 },
    { "varunlock", Nelson::MemoryGateway::varunlockBuiltin, 0, 2 },
    { "varislock", Nelson::MemoryGateway::varislockBuiltin, 1, 2 },
    { "memory", Nelson::MemoryGateway::memoryBuiltin, 2, 0 },
    { "isvar", Nelson::MemoryGateway::isvarBuiltin, 1, -1 },
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
