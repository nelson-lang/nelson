//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlcallBuiltin.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlcallBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_DLSYM_CATEGORY_STR) {
        Error(_W("dlsym handle expected."));
    }
    auto* dlsymObj = (DynamicLinkSymbolObject*)param1.getContentAsHandleScalar();
    ArrayOfVector params;
    for (size_t l = 1; l < argIn.size(); l++) {
        params.push_back(argIn[l]);
    }
    retval = dlsymObj->call(eval, nLhs, params);
    return retval;
}
//=============================================================================
