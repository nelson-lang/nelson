//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "whosBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Whos.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::whosBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    std::wstring filename;
    Nelson::SCOPE_LEVEL scope = Nelson::SCOPE_LEVEL::LOCAL_SCOPE;
    stringVector names;
    bool onlyGlobal = false;
    switch (argIn.size()) {
    case 0: {
        filename.clear();
        names.clear();
    } break;
    case 1: {
        std::string param = argIn[0].getContentAsCString();
        if (param == "global") {
            onlyGlobal = true;
        } else if (param == "-file") {
            Error(_W("filename expected after '-file'."));
        } else {
            names.push_back(param);
        }
    } break;
    default: {
        std::string param1 = argIn[0].getContentAsCString();
        indexType start = 1;
        if (param1 == "global") {
            onlyGlobal = true;
        } else if (param1 == "-file") {
            filename = argIn[1].getContentAsWideString();
            start++;
        } else {
            names.push_back(param1);
        }
        for (indexType k = start; k < (indexType)argIn.size(); ++k) {
            std::string param = argIn[k].getContentAsCString();
            if (param == "-file") {
                Error(_W("-file must be the first argument."));
            }
            names.push_back(param);
        }
    } break;
    }
    ArrayOf res = Whos(eval, filename, onlyGlobal, names, nLhs == 1);
    if (nLhs == 1) {
        retval << res;
    }
    return retval;
}
//=============================================================================
