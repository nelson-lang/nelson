//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "applicationExecutableBuiltin.hpp"
#include "ApplicationExecutable.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <stdexcept>
//=============================================================================
namespace Nelson::CompilerGateway {
ArrayOfVector
applicationExecutableBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    const auto operation = argIn[0].asWideString();
    nargoutcheck(nLhs, 0, operation == L"launcher-contract" ? 1 : 0);
    try {
        if (operation == L"launcher-contract") {
            nargincheck(argIn, 2, 2);
            return { ArrayOf::characterArray(
                applicationLauncherContract(argIn[1].asUtf8String())) };
        } else if (operation == L"wrap-installer" || operation == L"wrap-runtime-installer") {
            nargincheck(argIn, 4, 4);
            wrapApplicationInstaller(argIn[1].asWideString(), argIn[2].asWideString(),
                argIn[3].asWideString(), operation == L"wrap-runtime-installer");
        } else if (operation == L"set-icon") {
            nargincheck(argIn, 3, 3);
            setApplicationIcon(argIn[1].asWideString(), argIn[2].asWideStringVector());
        } else if (operation == L"validate-log-file") {
            nargincheck(argIn, 2, 2);
            validateApplicationRuntimeLog(argIn[1].asUtf8String());
        } else if (operation == L"set-log-file") {
            nargincheck(argIn, 3, 3);
            setApplicationRuntimeLog(argIn[1].asUtf8String(), argIn[2].asUtf8String());
        } else if (operation == L"validate-version") {
            nargincheck(argIn, 2, 2);
            parseApplicationVersion(argIn[1].asWideString());
        } else if (operation == L"set-version") {
            nargincheck(argIn, 3, 3);
            setApplicationVersion(argIn[1].asWideString(), argIn[2].asWideString());
        } else if (operation == L"recover-publication") {
            nargincheck(argIn, 2, 2);
            recoverApplicationFiles(argIn[1].asWideString());
        } else if (operation == L"publish") {
            nargincheck(argIn, 3, 4);
            if (argIn.size() == 4) {
                replaceApplicationFiles(
                    argIn[1].asWideString(), argIn[2].asWideString(), argIn[3].asWideString());
            } else {
                publishApplicationFiles(argIn[1].asWideString(), argIn[2].asWideString());
            }
        } else {
            Error(L"Nelson:nelsonc:invalidOperation", L"Unknown application executable operation.");
        }
    } catch (const std::invalid_argument& error) {
        RaiseError(error.what(), "Nelson:nelsonc:options");
    } catch (const std::exception& error) {
        RaiseError(error.what(), "Nelson:nelsonc:executable");
    }
    return {};
}
//=============================================================================
} // namespace Nelson::CompilerGateway
//=============================================================================
