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
#include "falseBuiltin.hpp"
#include "logicalBuiltin.hpp"
#include "trueBuiltin.hpp"
#include "xorBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"logical";
//=============================================================================
static const nlsGateway gateway[] = {
    { "logical", (ptrBuiltin)Nelson::LogicalGateway::logicalBuiltin, 1, 1 },
    { "true", (ptrBuiltin)Nelson::LogicalGateway::trueBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "false", (ptrBuiltin)Nelson::LogicalGateway::falseBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "xor", (ptrBuiltin)Nelson::LogicalGateway::xorBuiltin, 1, 2 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
