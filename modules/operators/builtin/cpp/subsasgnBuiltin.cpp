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
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
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
        raiseError(L"Nelson:operators:ERROR_SECOND_ARGUMENT_MUST_BE_A_STRUCTURE_WITH_TWO_FIELDS_"
                   L"WHOSE_NAMES_ARE_TYPE_AND",
            ERROR_SECOND_ARGUMENT_MUST_BE_A_STRUCTURE_WITH_TWO_FIELDS_WHOSE_NAMES_ARE_TYPE_AND,
            L"type", L"subs");
    }
    stringVector fieldnames = argIn[1].getFieldNames();
    bool isSupportedStruct = (fieldnames.size() == 2)
        && ((fieldnames[0] == "type" && fieldnames[1] == "subs")
            || (fieldnames[0] == "subs" && fieldnames[1] == "type"));
    if (!isSupportedStruct) {
        raiseError(L"Nelson:operators:ERROR_SECOND_ARGUMENT_MUST_BE_A_STRUCTURE_WITH_TWO_FIELDS_"
                   L"WHOSE_NAMES_ARE_TYPE_AND",
            ERROR_SECOND_ARGUMENT_MUST_BE_A_STRUCTURE_WITH_TWO_FIELDS_WHOSE_NAMES_ARE_TYPE_AND,
            L"type", L"subs");
    }

    ArrayOfVector typeFields = argIn[1].getFieldAsList("type");
    ArrayOfVector subsFields = argIn[1].getFieldAsList("subs");

    ArrayOf A(argIn[0]);
    ArrayOf B = argIn[2];

    if (typeFields.size() > 1) {
        raiseError(L"Nelson:operators:ERROR_NO_SUPPORT_FOR_INDEX_CHAINING",
            ERROR_NO_SUPPORT_FOR_INDEX_CHAINING);
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
            raiseError(L"Nelson:operators:ERROR_SUBS_FIELD_MUST_BE_A_CELL_ARRAY_OR_STRING",
                ERROR_SUBS_FIELD_MUST_BE_A_CELL_ARRAY_OR_STRING);
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
                raiseError(L"Nelson:operators:ERROR_FIRST_ARGUMENT_MUST_BE_A_STRUCT",
                    ERROR_FIRST_ARGUMENT_MUST_BE_A_STRUCT);
            }
            A.setField(name, B);
        } else if ((typeStr == "{}")) {
            if (indexAsVector.size() == 1) {
                A.setVectorContents(indexAsVector[0], B);
            } else {
                A.setNDimContents(indexAsVector, B);
            }
        } else {
            raiseError(
                L"Nelson:operators:ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_OR_EXPECTED",
                ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_OR_EXPECTED);
        }
    }
    retval.push_back(A);
    return retval;
}
//=============================================================================
