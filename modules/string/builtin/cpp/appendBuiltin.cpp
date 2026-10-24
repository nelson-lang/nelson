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
#include "appendBuiltin.hpp"
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
Nelson::StringGateway::appendBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    bool containsCellOrStringInput = false;
    bool containsStringInput = false;
    ArrayOfVector theInputs = argIn;
    for (auto& theInput : theInputs) {
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
        } else if (!theInput.isCharacterArray() && !theInput.isCell()) {
            Error(L"Nelson:string:invalidInputType",
                L"All inputs must be strings, character vectors, or cell arrays of character "
                L"vectors.");
        }
    }
    if (!containsStringInput && !containsCellOrStringInput) {
        std::wstring strs;
        for (auto& theInput : theInputs) {
            strs = strs.append(theInput.asCharacterArrayWideString());
        }
        retval << ArrayOf::characterArray(strs);
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

        for (const auto& v : theInputs) {
            if (v.isCharacterArray()) {
                std::wstring str = v.asWideString();
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
