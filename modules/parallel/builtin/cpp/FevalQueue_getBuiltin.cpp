//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalQueue_getBuiltin.hpp"
#include "FevalQueueObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::FevalQueue_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval(1);
    if (param1.getHandleCategory() != NLS_HANDLE_FEVALQUEUE_CATEGORY_STR) {
        raiseError(
            L"Nelson:parallel:ERROR_FEVALQUEUE_HANDLE_EXPECTED", ERROR_FEVALQUEUE_HANDLE_EXPECTED);
    }
    auto* objFevalQueue = (FevalQueueObject*)param1.getContentAsHandleScalar();
    ArrayOf res;
    if (!objFevalQueue->get(propertyName, res)) {
        raiseError2(_E("nelson:validators:invalidValueAtPosition"), 2);
    }
    retval << res;
    return retval;
}
//=============================================================================
