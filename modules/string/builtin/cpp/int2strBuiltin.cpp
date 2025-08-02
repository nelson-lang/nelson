//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "int2strBuiltin.hpp"
#include "Error.hpp"
#include "IntegerToString.hpp"
#include "VertCat.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
StringVectorToString(wstringVector V, Dimensions& DimsV)
{
    ArrayOf strArr;
    if (V.size() == 0) {
        Dimensions dims(1, 0);
        strArr = ArrayOf::emptyConstructor(dims);
        strArr.promoteType(NLS_CHAR);
    } else {
        if (V.size() == 1) {
            strArr = ArrayOf::characterArrayConstructor(V[0]);
        } else {
            size_t lenMax = 0;
            for (auto& k : V) {
                lenMax = std::max(lenMax, k.size());
            }
            std::wstring line;
            size_t q = 0;
            size_t R = DimsV[0];
            size_t C = DimsV.getElementCount() / DimsV[0];
            for (size_t r = 0; r < R; r++) {
                for (size_t c = 0; c < C; c++) {
                    size_t m = r + c * R;
                    std::wstring spaces = L"  ";
                    if (lenMax > V[m].size()) {
                        for (size_t l = 0; l < lenMax - V[m].size(); l++) {
                            spaces.push_back(L' ');
                        }
                    }
                    line = line + spaces + V[m];
                    if (q == C - 1) {
                        if (r == 0) {
                            strArr = ArrayOf::characterArrayConstructor(line);
                        } else {
                            ArrayOfVector argIn;
                            argIn << strArr;
                            argIn << ArrayOf::characterArrayConstructor(line);
                            strArr = VertCat(argIn, NLS_CHAR);
                        }
                        line.clear();
                        q = 0;
                    } else {
                        q++;
                    }
                }
            }
        }
    }
    return strArr;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::int2strBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    wstringVector result;
    std::wstring error_message;
    bool bRes = IntegerToString(argIn[0], result, error_message);
    if (bRes) {
        Dimensions dims = argIn[0].getDimensions();
        retval << StringVectorToString(result, dims);
    } else {
        Error(error_message);
    }
    return retval;
}
//=============================================================================
