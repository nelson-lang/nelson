//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strfindBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringFind.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strfindBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (!(argIn.size() == 2 || argIn.size() == 4)) {
        raiseError2(_E("nelson:arguments:wrongNumberOfInputs"));
    }
    bool forceAsCell = false;
    if (argIn.size() == 4) {
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        if (param3.isRowVectorCharacterArray() || (param3.isStringArray() && param3.isScalar())) {
            std::wstring str = param3.getContentAsWideString();
            if (str != L"ForceCellOutput") {
                raiseError(L"Nelson:string:ERROR_FORCECELLOUTPUT_EXPECTED",
                    ERROR_FORCECELLOUTPUT_EXPECTED);
            }
        } else {
            raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), NLS_STRING_ARRAY_STR, 3);
        }
        if (param4.isScalar() && param4.isLogical()) {
            forceAsCell = (param4.getContentAsLogicalScalar() != 0u);
        } else {
            raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), NLS_LOGICAL_STR, 4);
        }
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (!(A.isRowVectorCharacterArray() || A.isStringArray() || A.isCell() || A.isNumeric())) {
        raiseError2(_E("nelson:validators:mustBeTextAtPosition"), 1);
    }
    if (A.isRowVectorCharacterArray() || A.isStringArray() || A.isCell() || A.isNumeric()) {
        if (B.isRowVectorCharacterArray() || (B.isStringArray() && B.isScalar()) || B.isNumeric()) {
            if (A.isRowVectorCharacterArray() || (A.isStringArray() && A.isScalar())) {
                if (B.isRowVectorCharacterArray() || (B.isStringArray() && B.isScalar())) {
                    if ((B.isRowVector() && !B.isEmpty()) || B.isEmpty(true)) {
                        if (forceAsCell) {
                            Dimensions dimA(1, 1);
                            size_t nbElements = 1;
                            ArrayOf* elements = nullptr;
                            try {
                                elements = new ArrayOf[nbElements];
                            } catch (const std::bad_alloc&) {
                                raiseError2(_E("nelson:runtime:outOfMemory"));
                            }
                            for (size_t k = 0; k < nbElements; k++) {
                                // ArrayOf *cellA = (ArrayOf*)(A.getDataPointer());
                                elements[k] = StringFind(
                                    A.getContentAsWideString(), B.getContentAsWideString());
                            }
                            retval << ArrayOf(NLS_CELL_ARRAY, dimA, elements);
                        } else {
                            retval << StringFind(
                                A.getContentAsWideString(), B.getContentAsWideString());
                        }
                    } else {
                        raiseError(L"Nelson:string:ERROR_SECOND_ARGUMENT_A_SINGLE_STRING_EXPECTED",
                            ERROR_SECOND_ARGUMENT_A_SINGLE_STRING_EXPECTED);
                    }
                } else {
                    retval << ArrayOf::emptyConstructor();
                }
            } else if (A.isCell() || A.isStringArray()) {
                Dimensions dimA = A.getDimensions();
                size_t nbElements = dimA.getElementCount();
                ArrayOf* elements = nullptr;
                try {
                    elements = new ArrayOf[nbElements];
                } catch (const std::bad_alloc&) {
                    raiseError2(_E("nelson:runtime:outOfMemory"));
                }
                for (size_t k = 0; k < nbElements; k++) {
                    auto* cellA = (ArrayOf*)(A.getDataPointer());
                    if (cellA[k].isRowVectorCharacterArray()) {
                        if (B.isRowVectorCharacterArray() || (B.isStringArray() && B.isScalar())) {
                            if ((B.isRowVector() && !B.isEmpty()) || B.isEmpty(true)) {
                                std::wstring valB = B.getContentAsWideString();
                                elements[k] = StringFind(cellA[k].getContentAsWideString(), valB);
                            } else {
                                raiseError(
                                    L"Nelson:string:ERROR_SECOND_ARGUMENT_A_SINGLE_STRING_EXPECTED",
                                    ERROR_SECOND_ARGUMENT_A_SINGLE_STRING_EXPECTED);
                            }
                        } else {
                            elements[k] = ArrayOf::emptyConstructor();
                        }
                    } else {
                        if (A.isStringArray()) {
                            elements[k] = ArrayOf::emptyConstructor();
                        } else {
                            raiseError(L"Nelson:string:ERROR_FIRST_ARGUMENT_MUST_BE_CELL_OF_"
                                       L"STRINGS_OR_STRING",
                                ERROR_FIRST_ARGUMENT_MUST_BE_CELL_OF_STRINGS_OR_STRING);
                        }
                    }
                }
                retval << ArrayOf(NLS_CELL_ARRAY, dimA, elements);
            } else if (A.isNumeric()) {
                if ((A.isRowVector() && !A.isEmpty()) || A.isScalar() || A.isEmpty(true)) {
                    retval << ArrayOf::emptyConstructor();
                } else {
                    raiseError(L"Nelson:string:ERROR_INPUT_STRINGS_MUST_HAVE_ONE_ROW",
                        ERROR_INPUT_STRINGS_MUST_HAVE_ONE_ROW);
                }
            } else {
                raiseError(L"Nelson:string:ERROR_FIRST_ARGUMENT_MUST_BE_CELL_OF_STRINGS_OR_STRING",
                    ERROR_FIRST_ARGUMENT_MUST_BE_CELL_OF_STRINGS_OR_STRING);
            }
        } else {
            raiseError(L"Nelson:string:ERROR_SECOND_ARGUMENT_A_SINGLE_STRING_EXPECTED",
                ERROR_SECOND_ARGUMENT_A_SINGLE_STRING_EXPECTED);
        }
    } else {
        raiseError(L"Nelson:string:ERROR_FIRST_ARGUMENT_MUST_BE_CELL_OF_STRINGS_OR_STRING",
            ERROR_FIRST_ARGUMENT_MUST_BE_CELL_OF_STRINGS_OR_STRING);
    }
    return retval;
}
//=============================================================================
