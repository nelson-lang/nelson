//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "StringHelpers.hpp"
#include "UuidHelpers.hpp"
#include "strcatBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ConvertStringsToChars.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOfView.hpp"
#include "MutableArrayOfView.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strcatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    bool containsCellOrStringInput = false;
    bool containsStringInput = false;
    ArrayOfVector theInputs = argIn;
    std::vector<NelsonType> classType;
    for (auto& theInput : theInputs) {
        classType.push_back(theInput.dataClass());
        ArrayOf inputElement = theInput;
        containsCellOrStringInput = containsCellOrStringInput || inputElement.isCell();
        if (inputElement.isStringArray()) {
            containsStringInput = true;
            containsCellOrStringInput = true;
            theInput = ConvertStringsToChars(theInput, true);
        } else if (inputElement.isCell()) {
            if (inputElement.isScalar()) {
                ArrayOfView<ArrayOf> cells(inputElement, ArrayOfViewAssumeValidated {});
                theInput = cells[0];
            }
        } else if (inputElement.isEmpty()) {
            theInput = ArrayOf::characterArray(L"");
        } else if (theInput.isNumeric() || theInput.isLogical()) {
            // numbers are taken as character codes
            theInput.promoteType(NLS_CHAR);
        } else if (!theInput.isCharacterArray() && !theInput.isCell()) {
            Error(L"Nelson:string:invalidInputType",
                L"All inputs must be strings, character vectors, or cell arrays of character "
                L"vectors.");
        }
    }
    if (!containsStringInput && !containsCellOrStringInput) {
        // character inputs: a character matrix is concatenated row by row, a
        // single row is reused for every row, trailing blanks of each piece
        // are dropped
        indexType rows = 1;
        for (auto& theInput : theInputs) {
            indexType r = theInput.is2D() ? theInput.dimensions().rows() : 1;
            if (r > 1) {
                if (rows > 1 && r != rows) {
                    Error(L"Nelson:string:strcat:rowCountMismatch",
                        L"All character array inputs must have the same number of rows.");
                }
                rows = r;
            }
        }
        if (rows == 1) {
            std::wstring strs;
            for (auto& theInput : theInputs) {
                strs = strs.append(
                    StringHelpers::trim_right_copy(theInput.asCharacterArrayWideString()));
            }
            retval << ArrayOf::characterArray(strs);
        } else {
            std::vector<std::wstring> lines(rows);
            for (auto& theInput : theInputs) {
                wstringVector pieces = theInput.asWideStringVector(false);
                for (indexType r = 0; r < rows; ++r) {
                    std::wstring piece = pieces.size() == 1 ? pieces[0] : pieces[r];
                    StringHelpers::trim_right(piece);
                    lines[r].append(piece);
                }
            }
            retval << ArrayOf::characterArrayFromRows(lines, false);
        }
    } else {
        std::wstring missing_str;
        UuidHelpers::generateUuid(missing_str);

        Dimensions dimsOutput;
        bool haveDimsOutput = false;
        for (const auto& theInput : theInputs) {
            if (!haveDimsOutput) {
                if (!theInput.isCharacterArray()) {
                    dimsOutput = theInput.dimensions();
                    haveDimsOutput = true;
                }
            } else {
                if (!theInput.isCharacterArray() && !theInput.isScalar()) {
                    Dimensions dims = theInput.dimensions();
                    if (!dims.equals(dimsOutput)) {
                        Error(L"Nelson:string:allStringCellArrayInputs",
                            L"All string and cell array inputs must be the same size or scalars.");
                    }
                }
            }
        }
        if (!haveDimsOutput) {
            if (!theInputs[0].isCharacterArray()) {
                dimsOutput = theInputs[0].dimensions();
            } else {
                dimsOutput = Dimensions(1, 1);
            }
        }
        NelsonType outputClass = containsStringInput ? NLS_STRING_ARRAY : NLS_CELL_ARRAY;
        indexType nbElements = dimsOutput.elementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(outputClass, nbElements);
        ArrayOf res = ArrayOf(outputClass, dimsOutput, elements);
        MutableArrayOfView<ArrayOf> resultElements(res, ArrayOfViewAssumeValidated {});
        std::vector<wstringVector> vectorOfStringVector;
        vectorOfStringVector.reserve(theInputs.size());

        for (indexType k = 0; k < theInputs.size(); ++k) {
            ArrayOf v = theInputs[k];
            NelsonType cl = classType[k];
            if (v.isCharacterArray()) {
                std::wstring str = v.asWideString();
                if (cl == NLS_CHAR) {
                    StringHelpers::trim_right(str);
                }
                wstringVector vstr;
                vstr.reserve(nbElements);
                for (indexType i = 0; i < nbElements; ++i) {
                    vstr.push_back(str);
                }
                vectorOfStringVector.push_back(vstr);
            } else if (v.isCell()) {
                ArrayOfView<ArrayOf> cells(v, ArrayOfViewAssumeValidated {});
                indexType nbCells = v.elementCount();
                wstringVector vstr;
                vstr.reserve(nbCells);
                for (indexType q = 0; q < nbCells; ++q) {
                    ArrayOf c = cells[q];
                    if (c.isCharacterArray()) {
                        std::wstring s = c.asWideString();
                        if (cl == NLS_CHAR) {
                            StringHelpers::trim_right(s);
                        }
                        vstr.push_back(s);
                    } else {
                        if (c.isDoubleType(true)) {
                            if (std::isnan(c.asDoubleScalar())) {
                                vstr.push_back(missing_str);
                            } else if (c.isEmpty()) {
                                vstr.push_back(L"");
                            } else {
                                Error(L"Nelson:string:charVectorCellStringsExpected",
                                    L"character vector or cell of strings expected.");
                            }
                        } else if (c.isEmpty()) {
                            vstr.push_back(L"");
                        } else {
                            Error(L"Nelson:string:charVectorCellStringsExpected",
                                L"character vector or cell of strings expected.");
                        }
                    }
                }
                vectorOfStringVector.push_back(vstr);
            } else {
                Error(L"Nelson:string:invalidInputType",
                    L"All inputs must be strings, character vectors, or cell arrays of character "
                    L"vectors.");
            }
        }

        wstringVector resultAsVector;
        resultAsVector.reserve(nbElements);
        for (indexType m = 0; m < nbElements; ++m) {
            std::wstring r;
            for (indexType k = 0; k < theInputs.size(); ++k) {
                if (r.compare(missing_str) != 0) {
                    std::wstring v = vectorOfStringVector[k][m];
                    if (v.compare(missing_str) == 0) {
                        r = missing_str;
                    } else {
                        r = r.append(v);
                    }
                }
            }
            resultAsVector.push_back(r);
        }
        for (indexType k = 0; k < nbElements; ++k) {
            if (resultAsVector[k].compare(missing_str) == 0) {
                resultElements[k] = ArrayOf::doubleScalar(std::nan(""));
            } else {
                resultElements[k] = ArrayOf::characterArray(resultAsVector[k]);
            }
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
