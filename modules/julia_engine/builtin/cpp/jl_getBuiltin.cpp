//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "jl_getBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaObjectHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::jl_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    HandleGenericObject* hgo = argIn[0].asHandleScalar();
    if (hgo && hgo->category() == NLS_HANDLE_JULIA_CATEGORY_STR) {
        std::wstring methodName = argIn[1].asWideString();
        JuliaObjectHandle* jlh = (JuliaObjectHandle*)hgo;
        ArrayOf res;
        if (!jlh->get(methodName, res)) {
            Error(L"Nelson:julia:invalidProperty", L"Invalid property: %s.", methodName);
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
