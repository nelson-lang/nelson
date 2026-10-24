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
#include "IJVBuiltin.hpp"
#include "fullBuiltin.hpp"
#include "nnzBuiltin.hpp"
#include "nzmaxBuiltin.hpp"
#include "spallocBuiltin.hpp"
#include "spconvertBuiltin.hpp"
#include "spdiagsBuiltin.hpp"
#include "sprankBuiltin.hpp"
#include "sparseBuiltin.hpp"
#include "symrcmBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"sparse";
//=============================================================================
static const nlsGateway gateway[] = {
    { "sparse", (ptrBuiltin)Nelson::SparseGateway::sparseBuiltin, 1, 6 },
    { "full", (ptrBuiltin)Nelson::SparseGateway::fullBuiltin, 1, 1 },
    { "nnz", (ptrBuiltin)Nelson::SparseGateway::nnzBuiltin, 1, 1 },
    { "nzmax", (ptrBuiltin)Nelson::SparseGateway::nzmaxBuiltin, 1, 1 },
    { "spalloc", (ptrBuiltin)Nelson::SparseGateway::spallocBuiltin, 1, 3 },
    { "spconvert", (ptrBuiltin)Nelson::SparseGateway::spconvertBuiltin, 1, 1 },
    { "spdiags", (ptrBuiltin)Nelson::SparseGateway::spdiagsBuiltin, 2, 4 },
    { "sprank", (ptrBuiltin)Nelson::SparseGateway::sprankBuiltin, 1, 1 },
    { "symrcm", (ptrBuiltin)Nelson::SparseGateway::symrcmBuiltin, 1, 1 },
    { "IJV", (ptrBuiltin)Nelson::SparseGateway::IJVBuiltin, -1, 1 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
