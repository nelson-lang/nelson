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
#include "containersMapBuiltin.hpp"
#include "keyHashBuiltin.hpp"
#include "keyMatchBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"dictionary";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__containers_map__", (ptrBuiltin)Nelson::DictionaryGateway::containersMapBuiltin, 1, 5,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "keyHash", (ptrBuiltin)Nelson::DictionaryGateway::keyHashBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "keyMatch", (ptrBuiltin)Nelson::DictionaryGateway::keyMatchBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },

};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
