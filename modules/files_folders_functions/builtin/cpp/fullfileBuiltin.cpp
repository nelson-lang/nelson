//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "fullfileBuiltin.hpp"
#include "Error.hpp"
#include "FullFile.hpp"
#include "ConvertStringsToChars.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::fullfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool containsCellOrStringInput = false;
    bool containsStringInput = false;
    ArrayOfVector theInputs = argIn;
    for (auto& theInput : theInputs) {
        ArrayOf inputElement = theInput;
        containsCellOrStringInput = containsCellOrStringInput || inputElement.isCell();
        if (inputElement.isStringArray()) {
            containsStringInput = true;
            containsCellOrStringInput = true;
            theInput = ConvertStringsToChars(theInput);
        }
        if (inputElement.isCell() && inputElement.isScalar()) {
            ArrayOf* cells = (ArrayOf*)inputElement.getDataPointer();
            theInput = cells[0];
        }
        if (!theInput.isCharacterArray() && !theInput.isCell()) {
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
        retval.push_back(ArrayOf::characterArrayConstructor(FullFile(strs)));
    } else {
        Dimensions dimsOutput;
        bool haveDimsOutput = false;
        for (indexType k = 0; k < theInputs.size(); ++k) {
            if (k == 0) {
                if (!theInputs[k].isCharacterArray() && !theInputs[k].isScalar()) {
                    dimsOutput = theInputs[k].getDimensions();
                    haveDimsOutput = true;
                }
            } else {
                if (!haveDimsOutput) {
                    if (!theInputs[k].isCharacterArray() && !theInputs[k].isScalar()) {
                        dimsOutput = theInputs[k].getDimensions();
                        haveDimsOutput = true;
                    }
                } else {
                    if (!theInputs[k].isCharacterArray() && !theInputs[k].isScalar()) {
                        Dimensions dims = theInputs[k].getDimensions();
                        if (!dims.equals(dimsOutput)) {
                            Error(_W("All string and cell array inputs must be the same size or "
                                     "scalars."));
                        }
                    }
                }
            }
        }
        Class outputClass = containsStringInput ? NLS_STRING_ARRAY : NLS_CELL_ARRAY;
        indexType nbElements = dimsOutput.getElementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(outputClass, nbElements);
        ArrayOf res = ArrayOf(outputClass, dimsOutput, elements);
        std::vector<wstringVector> vectorOfStringVector;
        vectorOfStringVector.reserve(theInputs.size());

        for (auto v : theInputs) {
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
                indexType nbCells = v.getDimensions().getElementCount();
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
            } else if (v.isStringArray()) {
                auto* cells = (ArrayOf*)v.getDataPointer();
                indexType nbCells = v.getDimensions().getElementCount();
                wstringVector vstr;
                vstr.reserve(nbCells);
                for (indexType q = 0; q < nbCells; ++q) {
                    ArrayOf c = cells[q];
                    if (c.isCharacterArray()) {
                        vstr.push_back(c.getContentAsWideString());
                    } else {
                        vstr.push_back(L"");
                    }
                    vectorOfStringVector.push_back(vstr);
                }
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
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
