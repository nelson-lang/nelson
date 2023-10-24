//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "class_subsagnBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::class_subsagnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    if (!argIn[0].isClassType()) {
        Error(_W("Wrong type for argument #1. struct expected."));
    }
    std::string fieldname = argIn[1].getContentAsCString();
    ArrayOf res = argIn[0];
    ArrayOfVector value;
    value << argIn[2];
    res.setFieldAsList(fieldname, value);
    retval << res;
    return retval;
}
//=============================================================================
