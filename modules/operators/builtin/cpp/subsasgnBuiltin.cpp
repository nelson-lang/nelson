//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "subsasgnBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::subsasgnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 1, 1);

    if (!argIn[1].isStruct()) {
        Error(_("Second argument must be a structure with two fields whose names are 'type' and "
                "'subs'."));
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

    ArrayOf A(argIn[0]);
    ArrayOf B = argIn[2];

    if (typeFields.size() > 1) {
        Error(_("No support for index chaining."));
    }

    for (indexType k = 0; k < typeFields.size(); k++) {
        std::string typeStr = typeFields[k].getContentAsCString();
        ArrayOf subsField = subsFields[k];

        std::string name;
        ArrayOf index;
        bool isIndex = false;
        if ((subsField.isStringArray() && subsField.isScalar())
            || subsField.isRowVectorCharacterArray()) {
            name = subsField.getContentAsCString();
            isIndex = false;
        } else if (subsField.isCell() && subsField.isVector()) {
            isIndex = true;
            index = subsField;
        } else {
            Error(_("'subs' field must be a cell array or string."));
        }

        ArrayOfVector indexAsVector;

        if (isIndex) {
            ArrayOf* elements = (ArrayOf*)index.getDataPointer();
            for (indexType q = 0; q < index.getElementCount(); q++) {
                indexAsVector.push_back(elements[q]);
            }
        }
        if (typeStr == "()") {
            if (indexAsVector.size() == 1) {
                A.setVectorSubset(indexAsVector[0], B);
            } else {
                A.setNDimSubset(indexAsVector, B);
            }
        } else if (typeStr == ".") {
            if (!A.isStruct()) {
                Error(_("First argument must be a struct."));
            }
            A.setField(name, B);
        } else if ((typeStr == "{}")) {
            if (indexAsVector.size() == 1) {
                A.setVectorContents(indexAsVector[0], B);
            } else {
                A.setNDimContents(indexAsVector, B);
            }
        } else {
            Error(_("Illegal indexing structure argument: type '.', '{}' or '()' expected."));
        }
    }
    retval.push_back(A);
    return retval;
}
//=============================================================================
