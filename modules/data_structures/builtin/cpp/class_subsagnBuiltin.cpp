//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "class_subsagnBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
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
        raiseError(L"Nelson:data_structures:ERROR_WRONG_TYPE_ARG1_CLASS_EXPECTED",
            ERROR_WRONG_TYPE_ARG1_CLASS_EXPECTED);
    }
    if (!argIn[1].isStruct()) {
        raiseError2(_E("nelson:validators:mustBeStructAtPosition"), 2);
    }
    stringVector fieldnames = argIn[1].getFieldNames();
    bool isSupportedStruct = (fieldnames.size() == 2)
        && ((fieldnames[0] == "type" && fieldnames[1] == "subs")
            || (fieldnames[0] == "subs" && fieldnames[1] == "type"));
    if (!isSupportedStruct) {
        raiseError(L"Nelson:data_structures:ERROR_SECOND_ARGUMENT_MUST_BE_STRUCT_TYPE_SUBS",
            ERROR_SECOND_ARGUMENT_MUST_BE_STRUCT_TYPE_SUBS);
    }

    ArrayOfVector typeFields = argIn[1].getFieldAsList("type");
    ArrayOfVector subsFields = argIn[1].getFieldAsList("subs");

    if (typeFields.empty()) {
        raiseError(
            L"Nelson:data_structures:ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_DOT_EXPECTED",
            ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_DOT_EXPECTED);
    }
    std::string typeFieldStr = typeFields[0].getContentAsCString();
    if (typeFieldStr != ".") {
        raiseError(
            L"Nelson:data_structures:ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_DOT_EXPECTED",
            ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_DOT_EXPECTED);
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
