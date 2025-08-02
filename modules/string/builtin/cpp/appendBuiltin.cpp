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
#include "i18n.hpp"
#include "ConvertStringsToChars.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
                ArrayOf* cells = (ArrayOf*)inputElement.getDataPointer();
                theInput = cells[0];
            }
        } else if (inputElement.isEmpty()) {
            theInput = ArrayOf::characterArrayConstructor(L"");
        } else if (!theInput.isCharacterArray() && !theInput.isCell()) {
            Error(_W("All inputs must be strings, character vectors, or cell arrays of character "
                     "vectors."));
        }
    }
    if (!containsStringInput && !containsCellOrStringInput) {
        std::wstring strs;
        for (auto& theInput : theInputs) {
            strs = strs.append(theInput.getContentAsArrayOfCharacters());
        }
        retval << ArrayOf::characterArrayConstructor(strs);
    } else {
        std::wstring missing_str;
        UuidHelpers::generateUuid(missing_str);

        Dimensions dimsOutput;
        bool haveDimsOutput = false;
        for (const auto& theInput : theInputs) {
            if (!haveDimsOutput) {
                if (!theInput.isCharacterArray()) {
                    dimsOutput = theInput.getDimensions();
                    haveDimsOutput = true;
                }
            } else {
                if (!theInput.isCharacterArray() && !theInput.isScalar()) {
                    Dimensions dims = theInput.getDimensions();
                    if (!dims.equals(dimsOutput)) {
                        Error(_W(
                            "All string and cell array inputs must be the same size or scalars."));
                    }
                }
            }
        }
        if (!haveDimsOutput) {
            if (!theInputs[0].isCharacterArray()) {
                dimsOutput = theInputs[0].getDimensions();
            } else {
                dimsOutput = Dimensions(1, 1);
            }
        }
        NelsonType outputClass = containsStringInput ? NLS_STRING_ARRAY : NLS_CELL_ARRAY;
        indexType nbElements = dimsOutput.getElementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(outputClass, nbElements);
        ArrayOf res = ArrayOf(outputClass, dimsOutput, elements);
        std::vector<wstringVector> vectorOfStringVector;
        vectorOfStringVector.reserve(theInputs.size());

        for (const auto& v : theInputs) {
            if (v.isCharacterArray()) {
                std::wstring str = v.getContentAsWideString();
                wstringVector vstr;
                vstr.reserve(nbElements);
                for (indexType i = 0; i < nbElements; ++i) {
                    vstr.push_back(str);
                }
                vectorOfStringVector.push_back(vstr);
            } else if (v.isCell()) {
                auto* cells = (ArrayOf*)v.getDataPointer();
                indexType nbCells = v.getElementCount();
                wstringVector vstr;
                vstr.reserve(nbCells);
                for (indexType q = 0; q < nbCells; ++q) {
                    ArrayOf c = cells[q];
                    if (c.isCharacterArray()) {
                        std::wstring s = c.getContentAsWideString();
                        vstr.push_back(s);
                    } else {
                        if (c.isDoubleType(true)) {
                            if (std::isnan(c.getContentAsDoubleScalar())) {
                                vstr.push_back(missing_str);
                            } else if (c.isEmpty()) {
                                vstr.push_back(L"");
                            } else {
                                Error(_W("Cell of strings expected."));
                            }
                        } else if (c.isEmpty()) {
                            vstr.push_back(L"");
                        } else {
                            Error(_W("Cell of strings expected."));
                        }
                    }
                }
                vectorOfStringVector.push_back(vstr);
            } else {
                Error(_W("All inputs must be strings, character vectors, or cell arrays of "
                         "character vectors."));
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
                elements[k] = ArrayOf::doubleConstructor(std::nan("NaN"));
            } else {
                elements[k] = ArrayOf::characterArrayConstructor(resultAsVector[k]);
            }
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
