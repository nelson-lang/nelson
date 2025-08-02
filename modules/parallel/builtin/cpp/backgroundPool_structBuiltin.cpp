//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "backgroundPool_structBuiltin.hpp"
#include "BackgroundPoolObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::backgroundPool_structBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
        Error(_W("backgroundPool handle expected."));
    }
    auto* backgroundPool = (BackgroundPoolObject*)param1.getContentAsHandleScalar();
    wstringVector fieldnames = backgroundPool->getInstance()->fieldnames();
    ArrayOfVector fieldvalues;
    fieldvalues.reserve(fieldnames.size());
    for (const auto& name : fieldnames) {
        ArrayOf value;
        if (backgroundPool->getInstance()->get(name, value)) {
            fieldvalues.push_back(value);
        }
    }
    retval << ArrayOf::structScalarConstructor(fieldnames, fieldvalues);
    return retval;
}
//=============================================================================
