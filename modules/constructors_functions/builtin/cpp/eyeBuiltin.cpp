//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "eyeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Eye.hpp"
#include "StringToClass.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::eyeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    bool bIsSparse = false;
    NelsonType destClass = NLS_DOUBLE;
    indexType n = 1;
    indexType m = 1;
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    sizeType nRhs = argIn.size();
    if (nRhs == 0) {
        m = 1;
        n = 1;
        destClass = NLS_DOUBLE;
        bIsSparse = false;
    } else {
        ArrayOf lastarg = argIn[nRhs - 1];
        if (lastarg.isRowVectorCharacterArray()) {
            std::wstring strarg = lastarg.getContentAsWideString();
            destClass = StringToClass(strarg);
            nRhs--;
        } else {
            double n = static_cast<double>(nRhs) - 2.;
            if (n >= 0) {
                indexType pos = argIn.size() - 2;
                if (argIn[pos].isRowVectorCharacterArray()) {
                    std::wstring arg = argIn[pos].getContentAsWideString();
                    if (arg == L"like") {
                        ArrayOf arg = argIn[pos + 1];
                        bIsSparse = arg.isSparse();
                        destClass = arg.getDataClass();
                        if (argIn.size() - 2 == 0) {
                            m = 1;
                            // n = 1;
                        }
                        nRhs = argIn.size() - 2;
                    } else {
                        wchar_t buffer[4096];
                        const std::wstring fmt = std::wstring(ERROR_WRONG_ARGUMENT_X_VALUE);
                        swprintf(buffer, 4096, fmt.c_str(), pos);
                        Error(std::wstring(buffer));
                    }
                }
            }
        }
    }
    if (nRhs == 1) {
        ArrayOf arg = argIn[0];
        arg.promoteType(NLS_DOUBLE);
        if (arg.isScalar()) {
            n = arg.getContentAsScalarIndex();
            m = n;
        } else if (arg.isRowVector()) {
            if (arg.getElementCount() == 2) {
                indexType* pIndex = arg.getContentAsIndexPointer();
                n = pIndex[0];
                m = pIndex[1];
                delete[] pIndex;
            } else {
                Error(_W("N-dimensional arrays are not supported."));
            }
        } else {
            Error(_W("Size vector should be a row vector with real elements."));
        }
    } else if (nRhs == 2) {
        ArrayOf arg1 = argIn[0];
        n = arg1.getContentAsScalarIndex();
        ArrayOf arg2 = argIn[1];
        m = arg2.getContentAsScalarIndex();
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    retval << Eye(n, m, destClass, bIsSparse);
    return retval;
}
//=============================================================================
