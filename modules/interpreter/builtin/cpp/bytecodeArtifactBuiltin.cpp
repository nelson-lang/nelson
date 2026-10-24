//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bytecodeArtifactBuiltin.hpp"
#include "BytecodeArtifact.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <stdexcept>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::bytecodeArtifactBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    (void)eval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 3);
    const auto operation = argIn[0].asWideString();
    const auto filename = argIn[1].asWideString();
    ArrayOfVector result;
    try {
        if (operation == L"compile") {
            nargincheck(argIn, 3, 3);
            nargoutcheck(nLhs, 0, 0);
            compileBytecodeArtifact(filename, argIn[2].asWideString());
        } else if (operation == L"inspect") {
            nargincheck(argIn, 2, 2);
            result << ArrayOf::characterArray(inspectBytecodeArtifact(filename));
        } else if (operation == L"verify") {
            nargincheck(argIn, 2, 2);
            nargoutcheck(nLhs, 0, 0);
            verifyApplicationRuntime(filename);
        } else {
            Error("Nelson:bytecode:invalidOperation", "Unknown bytecode artifact operation.");
        }
    } catch (const std::exception& error) {
        RaiseError(error.what(), "Nelson:bytecode:invalidArtifact");
    }
    return result;
}
//=============================================================================
