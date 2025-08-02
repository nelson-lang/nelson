//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fullfileBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FullFile.hpp"
#include "ConvertStringsToChars.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::fullfileBuiltin(int nLhs, const ArrayOfVector& argIn)
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
            theInput = ConvertStringsToChars(theInput, false);
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
        wstringVector strs;
        strs.reserve(theInputs.size());
        for (auto& theInput : theInputs) {
            strs.push_back(theInput.getContentAsArrayOfCharacters());
        }
        retval << ArrayOf::characterArrayConstructor(FullFile(strs));
    } else {
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
                        vstr.push_back(c.getContentAsWideString());
                    } else {
                        if (c.isEmpty()) {
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
            wstringVector r;
            r.reserve(theInputs.size());
            for (indexType k = 0; k < theInputs.size(); ++k) {
                r.push_back(vectorOfStringVector[k][m]);
            }
            resultAsVector.push_back(FullFile(r));
        }
        for (indexType k = 0; k < nbElements; ++k) {
            elements[k] = ArrayOf::characterArrayConstructor(resultAsVector[k]);
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
