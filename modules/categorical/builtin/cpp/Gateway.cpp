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
#include "categoricalBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"categorical";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__categorical_cellstr_codes__",
        (ptrBuiltin)Nelson::CategoricalGateway::categorical_cellstr_codesBuiltin, 3, 1 },
    { "__categorical_codes_compare__",
        (ptrBuiltin)Nelson::CategoricalGateway::categorical_codes_compareBuiltin, 1, 3 },
    { "__categorical_codes_count__",
        (ptrBuiltin)Nelson::CategoricalGateway::categorical_codes_countBuiltin, 1, 2 },
    { "__categorical_codes_isundefined__",
        (ptrBuiltin)Nelson::CategoricalGateway::categorical_codes_isundefinedBuiltin, 1, 1 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
