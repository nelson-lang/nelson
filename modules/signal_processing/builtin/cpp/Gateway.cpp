//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "__findpeaks__Builtin.hpp"
#include "__upfirdn__Builtin.hpp"
#include "hanningBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"signal_processing";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__findpeaks__", (ptrBuiltin)Nelson::SignalProcessingGateway::__findpeaks__Builtin, 2, -1,
        CPP_BUILTIN },
    { "__upfirdn__", (ptrBuiltin)Nelson::SignalProcessingGateway::__upfirdn__Builtin, 1, 5,
        CPP_BUILTIN },
    { "hanning", (ptrBuiltin)Nelson::SignalProcessingGateway::hanningBuiltin, 1, 2, CPP_BUILTIN },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
