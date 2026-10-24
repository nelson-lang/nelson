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
#include "parquetinfoBuiltin.hpp"
#include "parquetreadBuiltin.hpp"
#include "parquetwriteBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"parquet";
//=============================================================================
static const nlsGateway gateway[] = {
    { "_parquetread", (ptrBuiltin)Nelson::ParquetGateway::parquetreadBuiltin, 1, 4 },
    { "_parquetwrite", (ptrBuiltin)Nelson::ParquetGateway::parquetwriteBuiltin, 0, 3 },
    { "_parquetinfo", (ptrBuiltin)Nelson::ParquetGateway::parquetinfoBuiltin, 1, 1 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
