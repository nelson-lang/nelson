//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "prodBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Prod.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::prodBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    size_t nRhs = argIn.size();
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("prod");
    }
    indexType d = 0;
    std::wstring strtype = L"default";
    bool withnan = true;
    ArrayOf X = argIn[0];
    switch (nRhs) {
    case 1: {
        // NOTHING TO DO
    } break;
    case 2: {
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            std::wstring wstr = param2.getContentAsWideString();
            if ((wstr == L"double") || (wstr == L"native") || (wstr == L"default")) {
                strtype = wstr;
            } else if ((wstr == L"includenan") || (wstr == L"omitnan")) {
                if (wstr == L"includenan") {
                    withnan = true;
                } else {
                    withnan = false;
                }
            } else {
                Error(_W("Wrong value for #4 argument."));
            }
        } else {
            d = param2.getContentAsScalarIndex(false);
        }
    } break;
    case 3: {
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        if (param2.isRowVectorCharacterArray()) {
            std::wstring wstr1 = param2.getContentAsWideString();
            if ((wstr1 == L"double") || (wstr1 == L"native") || (wstr1 == L"default")) {
                strtype = wstr1;
            } else {
                Error(_W("Wrong value for #2 argument."));
            }
            std::wstring wstr2 = param3.getContentAsWideString();
            if ((wstr2 == L"includenan") || (wstr2 == L"omitnan")) {
                if (wstr2 == L"includenan") {
                    withnan = true;
                } else {
                    withnan = false;
                }
            } else {
                Error(_W("Wrong value for #3 argument."));
            }
        } else {
            d = param2.getContentAsScalarIndex(false);
            std::wstring wstr2 = param3.getContentAsWideString();
            if ((wstr2 == L"double") || (wstr2 == L"native") || (wstr2 == L"default")) {
                strtype = wstr2;
            } else if ((wstr2 == L"includenan") || (wstr2 == L"omitnan")) {
                if (wstr2 == L"includenan") {
                    withnan = true;
                } else {
                    withnan = false;
                }
            } else {
                Error(_W("Wrong value for #3 argument."));
            }
        }
    } break;
    case 4: {
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        d = param2.getContentAsScalarIndex(false);
        std::wstring wstr1 = param3.getContentAsWideString();
        std::wstring wstr2 = param4.getContentAsWideString();
        if ((wstr1 == L"double") || (wstr1 == L"native") || (wstr1 == L"default")) {
            strtype = wstr1;
        } else {
            Error(_W("Wrong value for #3 argument."));
        }
        if ((wstr2 == L"includenan") || (wstr2 == L"omitnan")) {
            if (wstr2 == L"includenan") {
                withnan = true;
            } else {
                withnan = false;
            }
        } else {
            Error(_W("Wrong value for #4 argument."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    retval << Prod(X, d, strtype, withnan);
    return retval;
}
//=============================================================================
