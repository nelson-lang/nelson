//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
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
