//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "py_getBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonObjectHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::py_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    HandleGenericObject* hgo = argIn[0].getContentAsHandleScalar();
    if (hgo && hgo->getCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        std::wstring methodName = argIn[1].getContentAsWideString();
        PythonObjectHandle* poh = (PythonObjectHandle*)hgo;
        ArrayOf res;
        if (!poh->get(methodName, res)) {
            Error(ERROR_WRONG_ARGUMENT_2_VALUE + L" " + methodName);
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
