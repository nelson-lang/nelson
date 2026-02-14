//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "jl_invokeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaObjectHandle.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::jl_invokeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;

    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0);
    HandleGenericObject* hgo = argIn[0].getContentAsHandleScalar();
    if (!hgo || hgo->getCategory() != NLS_HANDLE_JULIA_CATEGORY_STR) {
        raiseError(L"Nelson:julia_engine:ERROR_JULIA_OBJECT_EXPECTED", ERROR_JULIA_OBJECT_EXPECTED);
    }

    std::wstring methodname = argIn[1].getContentAsWideString();
    ArrayOfVector params;
    for (size_t k = 2; k < argIn.size(); k++) {
        params.push_back(argIn[k]);
    }

    Interface* io = nullptr;
    if (eval) {
        io = eval->getInterface();
    }

    JuliaObjectHandle* poh = (JuliaObjectHandle*)hgo;
    if (!poh->invoke(io, methodname, params, nLhs, retval)) {
        raiseError(
            L"Nelson:error_manager:ERROR_UNDEFINED_METHOD", ERROR_UNDEFINED_METHOD, methodname);
    }
    return retval;
}
//=============================================================================
