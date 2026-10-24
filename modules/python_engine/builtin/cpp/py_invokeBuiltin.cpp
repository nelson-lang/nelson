//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "py_invokeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonObjectHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::py_invokeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;

    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0);
    HandleGenericObject* hgo = argIn[0].asHandleScalar();
    if (!hgo || hgo->category() != NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        Error(L"Nelson:python:invalidObject", L"Python object expected.");
    }

    std::wstring methodname = argIn[1].asWideString();
    ArrayOfVector params;
    for (size_t k = 2; k < argIn.size(); k++) {
        params.push_back(argIn[k]);
    }

    PythonObjectHandle* poh = (PythonObjectHandle*)hgo;
    Interface* io = nullptr;
    if (eval) {
        io = eval->getInterface();
    }
    if (!poh->invoke(io, methodname, params, nLhs, retval)) {
        Error(L"Nelson:python:invalidMethod", L"Invalid method: %s.", methodname);
    }
    return retval;
}
//=============================================================================
