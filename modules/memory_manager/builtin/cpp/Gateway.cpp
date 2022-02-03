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
#include "whosBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"memory_manager";
//=============================================================================
static const nlsGateway gateway[] = { { "clear", (ptrBuiltin)Nelson::MemoryGateway::clearBuiltin, 0,
                                          1, CPP_BUILTIN_WITH_EVALUATOR },
    { "who", (ptrBuiltin)Nelson::MemoryGateway::whoBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "whos", (ptrBuiltin)Nelson::MemoryGateway::whosBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "global", (ptrBuiltin)Nelson::MemoryGateway::globalBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isglobal", (ptrBuiltin)Nelson::MemoryGateway::isglobalBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "persistent", (ptrBuiltin)Nelson::MemoryGateway::persistentBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "assignin", (ptrBuiltin)Nelson::MemoryGateway::assigninBuiltin, 0, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "acquirevar", (ptrBuiltin)Nelson::MemoryGateway::acquirevarBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "varlock", (ptrBuiltin)Nelson::MemoryGateway::varlockBuiltin, -1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "varunlock", (ptrBuiltin)Nelson::MemoryGateway::varunlockBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "varislock", (ptrBuiltin)Nelson::MemoryGateway::varislockBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isvar", (ptrBuiltin)Nelson::MemoryGateway::isvarBuiltin, 1, -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "memory", (ptrBuiltin)Nelson::MemoryGateway::memoryBuiltin, 2, 0, CPP_BUILTIN } };
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
