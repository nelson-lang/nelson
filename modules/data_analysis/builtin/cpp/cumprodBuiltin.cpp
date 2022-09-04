//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cumprodBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "CumProd.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::cumprodBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // B = cumsum(A)
    // B = cumsum(A, dim)
    // B = cumsum(A, [dim], nanflag)
    ArrayOfVector retval;
    size_t nRhs = argIn.size();
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "cumprod", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "cumprod", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        bool withNaN = true;
        bool needOverload = true;
        bool reverse = false;
        indexType n = 0;

        switch (nRhs) {
        case 1: {
            // NOTHING TO DO
        } break;
        case 2: {
            if (argIn[1].isRowVectorCharacterArray()) {
                std::wstring wstr = argIn[1].getContentAsWideString();
                if (wstr == L"includenan") {
                    withNaN = true;
                } else if (wstr == L"omitnan") {
                    withNaN = false;
                } else if (wstr == L"reverse") {
                    reverse = true;
                } else if (wstr == L"forward") {
                    reverse = false;
                } else {
                    Error(_W("Wrong value for #2 argument."));
                }
            } else {
                n = argIn[1].getContentAsScalarIndex(false);
            }
        } break;
        case 3: {
            if (argIn[1].isRowVectorCharacterArray()) {
                std::wstring wstr1 = argIn[1].getContentAsWideString();
                std::wstring wstr2 = argIn[2].getContentAsWideString();
                if (wstr1 == wstr2) {
                    Error(_W("Wrong value for #3 argument."));
                }
                if (wstr1 == L"includenan") {
                    withNaN = true;
                } else if (wstr1 == L"omitnan") {
                    withNaN = false;
                } else if (wstr1 == L"reverse") {
                    reverse = true;
                } else if (wstr1 == L"forward") {
                    reverse = false;
                } else {
                    Error(_W("Wrong value for #2 argument."));
                }
                if (wstr2 == L"includenan") {
                    withNaN = true;
                } else if (wstr2 == L"omitnan") {
                    withNaN = false;
                } else if (wstr2 == L"reverse") {
                    reverse = true;
                } else if (wstr2 == L"forward") {
                    reverse = false;
                } else {
                    Error(_W("Wrong value for #3 argument."));
                }
            } else {
                n = argIn[1].getContentAsScalarIndex(false);
                std::wstring wstr2 = argIn[2].getContentAsWideString();
                if (wstr2 == L"includenan") {
                    withNaN = true;
                } else if (wstr2 == L"omitnan") {
                    withNaN = false;
                } else if (wstr2 == L"reverse") {
                    reverse = true;
                } else if (wstr2 == L"forward") {
                    reverse = false;
                } else {
                    Error(_W("Wrong value for #3 argument."));
                }
            }
        } break;
        case 4: {
            n = argIn[1].getContentAsScalarIndex(false);
            std::wstring wstr1 = argIn[2].getContentAsWideString();
            std::wstring wstr2 = argIn[3].getContentAsWideString();
            if (wstr1 == wstr2) {
                Error(_W("Wrong value for #3 argument."));
            }
            if (wstr1 == L"includenan") {
                withNaN = true;
            } else if (wstr1 == L"omitnan") {
                withNaN = false;
            } else if (wstr1 == L"reverse") {
                reverse = true;
            } else if (wstr1 == L"forward") {
                reverse = false;
            } else {
                Error(_W("Wrong value for #2 argument."));
            }
            if (wstr2 == L"includenan") {
                withNaN = true;
            } else if (wstr2 == L"omitnan") {
                withNaN = false;
            } else if (wstr2 == L"reverse") {
                reverse = true;
            } else if (wstr2 == L"forward") {
                reverse = false;
            } else {
                Error(_W("Wrong value for #3 argument."));
            }
        } break;
        default: {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        } break;
        }

        ArrayOf res = CumProd(argIn[0], n, withNaN, reverse, needOverload);
        if (needOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "cumprod", bSuccess);
            if (!bSuccess) {
                OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION, "cumprod");
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
