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
#include "NargOut.hpp"
#include "Validators.hpp"
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
    nargincheck(argIn, 2, 4); //-V112

    switch (argIn.size()) {
    case 2: {
        if (nLhs == 1) {
            Error(_W("No output arguments are allowed if only two input arguments."));
        }
        Context* context = eval->getContext();
        if (context->getCurrentScope()->getName() == "base") {
            Error(_W("You can only call 'nargoutchk' from within a Nelson function."));
        }

        mustBeScalarOrEmpty(argIn, 0);
        mustBeNonempty(argIn, 0);
        mustBeInteger(argIn, 0);

        mustBeScalarOrEmpty(argIn, 1);
        mustBeNonempty(argIn, 1);
        mustBeInteger(argIn, 1);

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
        if (argIn.size() == 4) { //-V112
            std::wstring param4AsString = argIn[3].getContentAsWideString();
            if (param4AsString == L"struct") {
                asStruct = true;
            } else if (param4AsString == L"string") {
                asStruct = false;
            } else {
                Error(_W("#4 input must be either 'struct' or 'string'."));
            }
        }
        mustBeScalarOrEmpty(argIn, 0);
        mustBeNonempty(argIn, 0);
        mustBeInteger(argIn, 0);

        mustBeScalarOrEmpty(argIn, 1);
        mustBeNonempty(argIn, 1);
        mustBeInteger(argIn, 1);

        mustBeScalarOrEmpty(argIn, 2);
        mustBeNonempty(argIn, 2);
        mustBeInteger(argIn, 2);

        int minArgs = argIn[0].getContentAsInteger32Scalar();
        int maxArgs = argIn[1].getContentAsInteger32Scalar();
        int numArgs = argIn[2].getContentAsInteger32Scalar();

        std::wstring msg = L"";
        std::wstring id = L"";
        if (numArgs < minArgs) {
            msg = _W("Not enough output arguments.");
            id = L"Nelson:nargoutchk:notEnoughOutputs";
        }
        if (numArgs > maxArgs) {
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
    default: { } break; }

    return retval;
}
//=============================================================================
