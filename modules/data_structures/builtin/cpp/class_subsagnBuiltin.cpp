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
        Error(_W("Wrong type for argument #1. class expected."));
    }
    if (!argIn[1].isStruct()) {
        Error(_W("Wrong type for argument #2. struct expected."));
    }
    stringVector fieldnames = argIn[1].getFieldNames();
    bool isSupportedStruct = (fieldnames.size() == 2)
        && ((fieldnames[0] == "type" && fieldnames[1] == "subs")
            || (fieldnames[0] == "subs" && fieldnames[1] == "type"));
    if (!isSupportedStruct) {
        Error(_("Second argument must be a structure with two fields whose names are 'type' and "
                "'subs'."));
    }

    ArrayOfVector typeFields = argIn[1].getFieldAsList("type");
    ArrayOfVector subsFields = argIn[1].getFieldAsList("subs");

    if (typeFields.empty()) {
        Error(_("Illegal indexing structure argument: type '.' expected."));
    }
    std::string typeFieldStr = typeFields[0].getContentAsCString();
    if (typeFieldStr != ".") {
        Error(_("Illegal indexing structure argument: type '.' expected."));
    }
    std::string fieldname = subsFields[0].getContentAsCString();
    ArrayOf res = argIn[0];
    ArrayOfVector value;
    value << argIn[2];
    res.setFieldAsList(fieldname, value);
    retval << res;
    return retval;
}
//=============================================================================
