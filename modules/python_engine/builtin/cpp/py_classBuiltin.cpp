//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "py_classBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonObjectHandle.hpp"
#include "PythonEngine.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::py_classBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    HandleGenericObject* hgo = argIn[0].getContentAsHandleScalar();
    if (hgo && hgo->getCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        PythonObjectHandle* poh = (PythonObjectHandle*)hgo;
        retval << ArrayOf::characterArrayConstructor(poh->getClassName());
    }
    return retval;
}
//=============================================================================
