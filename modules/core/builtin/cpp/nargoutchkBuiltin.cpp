//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nargoutchkBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// nargoutchk(minArgs,maxArgs)
// msgText = nargoutchk(minArgs,maxArgs,numArgs)
// msgStruct = nargoutchk(minArgs,maxArgs,numArgs,'struct')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::nargoutchkBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 4);

    switch (argIn.size()) {
    case 2: {
        if (nLhs == 1) {
            Error(L"Nelson:nargoutchk:noOutputWithTwoInputs",
                L"No output arguments are allowed if only two input arguments.");
        }
        Context* context = eval->getContext();
        if (context->getCurrentScope()->getName() == "base") {
            Error(L"Nelson:core:notInFunction",
                L"You can only call '%s' from within a Nelson function.", L"nargoutchk");
        }
        int minArgs = argIn[0].asInt32Scalar();
        int maxArgs = argIn[1].asInt32Scalar();

        int nargout = context->getCurrentScope()->getNargOut();
        if (nargout < minArgs) {
            ErrorAsCaller(L"Nelson:nargoutchk:notEnoughOutputs", L"Not enough output arguments.");
        }
        if (nargout > maxArgs) {
            ErrorAsCaller(L"Nelson:nargoutchk:tooManyOutputs", L"Too many output arguments.");
        }
    } break;
    case 4:
    case 3: {
        bool asStruct = false;
        if (argIn.size() == 4) {
            std::wstring param4AsString = argIn[3].asWideString();
            if (param4AsString == L"struct") {
                asStruct = true;
            } else if (param4AsString == L"string") {
                asStruct = false;
            } else {
                Error(L"Nelson:nargoutchk:invalidInputType",
                    L"#%d input must be either 'struct' or 'string'.", 4);
            }
        }
        if (!argIn[0].isScalar() || !argIn[0].isNumeric()) {
            Error("Nelson:expectedScalar", "%s must be a scalar.", "Argument #1");
        }
        if (!argIn[1].isScalar() || !argIn[1].isNumeric()) {
            Error("Nelson:expectedScalar", "%s must be a scalar.", "Argument #2");
        }
        if (!argIn[2].isScalar() || !argIn[2].isNumeric()) {
            Error("Nelson:expectedScalar", "%s must be a scalar.", "Argument #3");
        }

        int minArgs = argIn[0].asInt32Scalar(false, true);

        bool maxArgsIsInf = false;
        if (argIn[1].isDoubleType(true)) {
            double maxValue = argIn[1].asDoubleScalar();
            maxArgsIsInf = std::isinf(maxValue);
        }
        int numArgs = argIn[2].asInt32Scalar(false, true);

        std::wstring msg = L"";
        std::wstring id = L"";
        if (numArgs < minArgs) {
            msg = Nelson::resolveMessageTemplate(
                L"Nelson:core:enoughOutput", L"Not enough output arguments.");
            id = L"Nelson:nargoutchk:notEnoughOutputs";
        }

        int maxArgs = argIn[1].asInt32Scalar(false, true);
        if (!maxArgsIsInf && numArgs > maxArgs) {
            msg = _W("Too many output arguments.");
            id = L"Nelson:nargoutchk:tooManyOutputs";
        }
        if (asStruct) {
            wstringVector fieldnames;
            ArrayOfVector fieldvalues;
            fieldnames.push_back(L"message");
            fieldnames.push_back(L"identifier");

            fieldvalues << ArrayOf::characterArray(msg);
            fieldvalues << ArrayOf::characterArray(id);

            retval << ArrayOf::structArray(fieldnames, fieldvalues);
        } else {
            retval << ArrayOf::characterArray(msg);
        }
    } break;
    default: {
    } break;
    }

    return retval;
}
//=============================================================================
