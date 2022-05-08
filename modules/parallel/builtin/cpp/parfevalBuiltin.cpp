//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "parfevalBuiltin.hpp"
#include "BackgroundPoolObject.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "FevalFutureObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::parfevalBuiltin(Evaluator *eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, std::numeric_limits<int>::max());
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()){
        Error(_W("backgroundPool handle expected."));
    }
    if (param1.getHandleCategory() != BACKGROUNDPOOL_CATEGORY_STR) {
        Error(_W("backgroundPool handle expected."));
    }
    ArrayOf param2 = argIn[1];
    if (!param2.isFunctionHandle()) {
        Error(_W("function handle handle expected."));
    }
    function_handle fh = param2.getContentAsFunctionHandle();
    if (fh.anonymous.empty() && fh.name.empty()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }
    std::string fname = fh.name;
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    ArrayOfVector args;
    for (size_t k = 3; k < argIn.size(); ++k) {
        args << argIn[k];
    }
    if (!context->lookupFunction(fname, funcDef)) {
        Error(_W("function \'") + utf8_to_wstring(fname) + _W("\' is not a function."));
    }
    if (funcDef->type() != NLS_BUILT_IN_FUNCTION) {
 //       Error(_W("Only builtin "));
    }

    ArrayOf param3 = argIn[2];
    bool isReal = param3.getDataClass() == NLS_DOUBLE || param3.getDataClass() == NLS_SINGLE;
    if (!isReal) {
        Error(_W("integer value expected."));
    }
    double value = param3.getContentAsDoubleScalar();
    int ivalue = (int)(value);
    if (value != (double)ivalue) {
        Error(_W("integer value expected."));
    }
    if (ivalue < 0) {
        Error(_W("non negative value expected."));
    }
    auto* backgroundPoolObject = (BackgroundPoolObject*)param1.getContentAsHandleScalar();
    FevalFutureObject *fevalFutureObj = backgroundPoolObject->feval(funcDef, ivalue, args);
    retval << ArrayOf::handleConstructor(fevalFutureObj);
    return retval;
}
//=============================================================================
