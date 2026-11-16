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
#include "native2unicodeBuiltin.hpp"
#include "unicode2nativeBuiltin.hpp"
#include "nativecharsetBuiltin.hpp"
#include "base64encodeBuiltin.hpp"
#include "base64decodeBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"characters_encoding";
//=============================================================================
static const nlsGateway gateway[] = {
    { "unicode2native", (ptrBuiltin)Nelson::CharactersEncodingGateway::unicode2nativeBuiltin, 1,
        2 },
    { "native2unicode", (ptrBuiltin)Nelson::CharactersEncodingGateway::native2unicodeBuiltin, 1,
        2 },
    { "nativecharset", (ptrBuiltin)Nelson::CharactersEncodingGateway::nativecharsetBuiltin, 1, 1 },
    { "__base64encode__", (ptrBuiltin)Nelson::CharactersEncodingGateway::base64encodeBuiltin, 1,
        1 },
    { "__base64decode__", (ptrBuiltin)Nelson::CharactersEncodingGateway::base64decodeBuiltin, 1,
        1 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
