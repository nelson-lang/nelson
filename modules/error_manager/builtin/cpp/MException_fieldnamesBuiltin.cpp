//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MException_fieldnamesBuiltin.hpp"
#include "MException.hpp"
#include "ClassName.hpp"
#include "ToCellString.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isClassStruct() && ClassName(argIn[0]) == "MException") {
        stringVector fieldnames = argIn[0].getFieldNames();
        retval << ToCellStringAsColumn(argIn[0].getFieldNames());
    } else {
        Error(_W("MException expected."));
    }
    return retval;
}
//=============================================================================
