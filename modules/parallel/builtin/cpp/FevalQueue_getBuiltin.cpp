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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
    std::wstring propertyName = param2.asWideString();
    ArrayOfVector retval(1);
    if (param1.handleCategory() != NLS_HANDLE_FEVALQUEUE_CATEGORY_STR) {
        Error(L"Nelson:fevalQueue:handleExpected", L"FevalQueue handle expected.");
    }
    auto* objFevalQueue = (FevalQueueObject*)param1.asHandleScalar();
    ArrayOf res;
    if (!objFevalQueue->get(propertyName, res)) {
        Error(L"Nelson:parallel:invalidProperty", L"Invalid property: %s.", propertyName);
    }
    retval << res;
    return retval;
}
//=============================================================================
