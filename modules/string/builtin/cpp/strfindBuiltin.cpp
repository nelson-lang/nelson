//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "strfindBuiltin.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringFind.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
findNumericSequence(const ArrayOf& A, const ArrayOf& B);
static bool
isNumericOrLogical(const ArrayOf& value);
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strfindBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 3) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    bool forceAsCell = false;
    if (argIn.size() == 4) {
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        if (param3.isRowVectorCharacterArray() || (param3.isStringArray() && param3.isScalar())) {
            std::wstring str = StringHelpers::to_lower_copy(param3.asWideString());
            if (str != L"forcecelloutput") {
                Error(L"Nelson:string:forceCellOutputExpectedThirdInputArgument",
                    L"'ForceCellOutput' expected as third input argument.");
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (3),
                L"string");
        }
        if (param4.isScalar() && (param4.isLogical() || param4.isNumeric())) {
            forceAsCell = param4.isLogical() ? (param4.asLogicalScalar() != 0u)
                                             : (param4.asDoubleScalar() != 0);
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (4),
                L"logical");
        }
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (B.isCell() && B.isScalar()) {
        // a scalar cell holds the pattern
        B = ((ArrayOf*)B.dataPointer())[0];
    }
    if (!(A.isRowVectorCharacterArray() || A.isStringArray() || A.isCell()
            || isNumericOrLogical(A))) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    if (A.isRowVectorCharacterArray() || A.isStringArray() || A.isCell() || isNumericOrLogical(A)) {
        if (B.isRowVectorCharacterArray() || (B.isStringArray() && B.isScalar())
            || isNumericOrLogical(B)) {
            if (A.isRowVectorCharacterArray() || (A.isStringArray() && A.isScalar())) {
                if (B.isRowVectorCharacterArray() || (B.isStringArray() && B.isScalar())) {
                    if ((B.isRowVector() && !B.isEmpty()) || B.isEmpty()) {
                        if (forceAsCell) {
                            Dimensions dimA(1, 1);
                            size_t nbElements = 1;
                            ArrayOf* elements = nullptr;
                            try {
                                elements = new ArrayOf[nbElements];
                            } catch (const std::bad_alloc&) {
                                Error(L"Nelson:outOfMemory",
                                    L"Memory allocation error... You may have run out of memory!");
                            }
                            for (size_t k = 0; k < nbElements; k++) {
                                elements[k] = StringFind(A.asWideString(), B.asWideString());
                            }
                            retval << ArrayOf(NLS_CELL_ARRAY, dimA, elements);
                        } else {
                            retval << StringFind(A.asWideString(), B.asWideString());
                        }
                    } else {
                        Error(L"Nelson:string:secondArgumentSingleStringExpected",
                            L"Second argument a single string expected.");
                    }
                } else {
                    retval << findNumericSequence(A, B);
                }
            } else if (A.isCell() || A.isStringArray()) {
                Dimensions dimA = A.dimensions();
                size_t nbElements = dimA.elementCount();
                ArrayOf* elements = nullptr;
                try {
                    elements = new ArrayOf[nbElements];
                } catch (const std::bad_alloc&) {
                    Error(L"Nelson:outOfMemory",
                        L"Memory allocation error... You may have run out of memory!");
                }
                for (size_t k = 0; k < nbElements; k++) {
                    auto* cellA = (ArrayOf*)(A.dataPointer());
                    if (cellA[k].isRowVectorCharacterArray()) {
                        if (B.isRowVectorCharacterArray() || (B.isStringArray() && B.isScalar())) {
                            if ((B.isRowVector() && !B.isEmpty()) || B.isEmpty(true)) {
                                std::wstring valB = B.asWideString();
                                elements[k] = StringFind(cellA[k].asWideString(), valB);
                            } else {
                                Error(L"Nelson:string:secondArgumentSingleStringExpected",
                                    L"Second argument a single string expected.");
                            }
                        } else if (B.isNumeric()) {
                            elements[k] = findNumericSequence(cellA[k], B);
                        } else {
                            elements[k] = ArrayOf::empty();
                        }
                    } else {
                        if (A.isStringArray()) {
                            elements[k] = ArrayOf::empty();
                        } else {
                            Error(L"Nelson:strfind:invalidInput",
                                L"First argument must be a cell of strings (or a string) and "
                                L"second argument a string.");
                        }
                    }
                }
                retval << ArrayOf(NLS_CELL_ARRAY, dimA, elements);
            } else if (isNumericOrLogical(A)) {
                if (A.isRowVector() || A.isScalar() || A.isEmpty(true)) {
                    retval << findNumericSequence(A, B);
                } else {
                    Error(L"Nelson:string:inputStringsOneRow", L"Input strings must have one row.");
                }
            } else {
                Error(L"Nelson:strfind:invalidInput",
                    L"First argument must be a cell of strings (or a string) and second argument a "
                    L"string.");
            }
        } else {
            Error(L"Nelson:string:secondArgumentSingleStringExpected",
                L"Second argument a single string expected.");
        }
    } else {
        Error(L"Nelson:strfind:invalidInput",
            L"First argument must be a cell of strings (or a string) and second argument a "
            L"string.");
    }
    return retval;
}
//=============================================================================
static bool
isNumericOrLogical(const ArrayOf& value)
{
    return value.isNumeric() || value.isLogical();
}
//=============================================================================
// Numeric (or mixed numeric / character) inputs are compared value by value,
// characters through their codes: strfind([1 2 3 2], 2) is [2 4].
static ArrayOf
findNumericSequence(const ArrayOf& A, const ArrayOf& B)
{
    if (B.isLogical() && A.isCharacterArray()) {
        Error(L"Nelson:string:secondArgumentSingleStringExpected",
            L"Second argument a single string expected.");
    }
    ArrayOf a(A.isStringArray() ? ArrayOf::characterArray(A.asWideString()) : A);
    ArrayOf b(B);
    if (b.isStringArray() || a.isComplex() || b.isComplex() || a.isEmpty() || b.isEmpty()
        || b.elementCount() > a.elementCount()) {
        return ArrayOf::empty();
    }
    a.promoteType(NLS_DOUBLE);
    b.promoteType(NLS_DOUBLE);
    const auto* pa = static_cast<const double*>(a.dataPointer());
    const auto* pb = static_cast<const double*>(b.dataPointer());
    indexType na = a.elementCount();
    indexType nb = b.elementCount();
    std::vector<double> positions;
    for (indexType k = 0; k + nb <= na; ++k) {
        indexType j = 0;
        while (j < nb && pa[k + j] == pb[j]) {
            ++j;
        }
        if (j == nb) {
            positions.push_back(static_cast<double>(k + 1));
        }
    }
    if (positions.empty()) {
        return ArrayOf::empty();
    }
    return ArrayOf::doubleRowVector(positions);
}
//=============================================================================
