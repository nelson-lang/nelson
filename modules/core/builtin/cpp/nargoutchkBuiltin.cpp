//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nargoutchkBuiltin.hpp"
#include "Error.hpp"
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
            Error(_W("No output arguments are allowed if only two input arguments."));
        }
        Context* context = eval->getContext();
        if (context->getCurrentScope()->getName() == "base") {
            Error(_W("You can only call 'nargoutchk' from within a Nelson function."));
        }
        int minArgs = argIn[0].getContentAsInteger32Scalar();
        int maxArgs = argIn[1].getContentAsInteger32Scalar();

        int nargout = context->getCurrentScope()->getNargOut();
        if (nargout < minArgs) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS, L"Nelson:nargoutchk:notEnoughOutputs", true);
        }
        if (nargout > maxArgs) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS, L"Nelson:nargoutchk:tooManyOutputs", true);
        }
    } break;
    case 4:
    case 3: {
        bool asStruct = false;
        if (argIn.size() == 4) {
            std::wstring param4AsString = argIn[3].getContentAsWideString();
            if (param4AsString == L"struct") {
                asStruct = true;
            } else if (param4AsString == L"string") {
                asStruct = false;
            } else {
                Error(_W("#4 input must be either 'struct' or 'string'."));
            }
        }
        if (!argIn[0].isScalar() || !argIn[0].isNumeric()) {
            Error(_("Scalar integer value required for #1 argument."));
        }
        if (!argIn[1].isScalar() || !argIn[1].isNumeric()) {
            Error(_("Scalar integer value required for #2 argument."));
        }
        if (!argIn[2].isScalar() || !argIn[2].isNumeric()) {
            Error(_("Scalar integer value required for #3 argument."));
        }

        int minArgs = argIn[0].getContentAsInteger32Scalar(false, true);

        bool maxArgsIsInf = false;
        if (argIn[1].isDoubleType(true)) {
            double maxValue = argIn[1].getContentAsDoubleScalar();
            maxArgsIsInf = std::isinf(maxValue);
        }
        int numArgs = argIn[2].getContentAsInteger32Scalar(false, true);

        std::wstring msg = L"";
        std::wstring id = L"";
        if (numArgs < minArgs) {
            msg = _W("Not enough output arguments.");
            id = L"Nelson:nargoutchk:notEnoughOutputs";
        }

        int maxArgs = argIn[1].getContentAsInteger32Scalar(false, true);
        if (!maxArgsIsInf && numArgs > maxArgs) {
            msg = _W("Too many output arguments.");
            id = L"Nelson:nargoutchk:tooManyOutputs";
        }
        if (asStruct) {
            wstringVector fieldnames;
            ArrayOfVector fieldvalues;
            fieldnames.push_back(L"message");
            fieldnames.push_back(L"identifier");

            fieldvalues << ArrayOf::characterArrayConstructor(msg);
            fieldvalues << ArrayOf::characterArrayConstructor(id);

            retval << ArrayOf::structConstructor(fieldnames, fieldvalues);
        } else {
            retval << ArrayOf::characterArrayConstructor(msg);
        }
    } break;
    default: {
    } break;
    }

    return retval;
}
//=============================================================================
