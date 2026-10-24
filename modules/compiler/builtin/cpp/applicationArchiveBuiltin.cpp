//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "applicationArchiveBuiltin.hpp"
#include "ApplicationArchiveBuilder.hpp"
#include "ApplicationArchiveFormat.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <stdexcept>
//=============================================================================
namespace Nelson::CompilerGateway {
ArrayOfVector
applicationArchiveBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 6);
    nargoutcheck(nLhs, 0, 0);
    const auto operation = argIn[0].asWideString();
    try {
        if (operation == L"create" || operation == L"create-external") {
            nargincheck(argIn, 5, 5);
            createApplicationArchive(argIn[1].asWideString(), argIn[2].asWideString(),
                argIn[3].asWideString(), argIn[4].asLogicalScalar(), {}, operation == L"create");
        } else if (operation == L"create-installed" || operation == L"create-external-installed") {
            nargincheck(argIn, 6, 6);
            const auto fingerprint = argIn[5].asUtf8String();
            ApplicationArchive::require(ApplicationArchive::validDigest(fingerprint),
                "Invalid application engine fingerprint.");
            createApplicationArchive(argIn[1].asWideString(), argIn[2].asWideString(),
                argIn[3].asWideString(), argIn[4].asLogicalScalar(), fingerprint,
                operation == L"create-installed");
        } else if (operation == L"pack") {
            nargincheck(argIn, 4, 4);
            packApplicationArchive(
                argIn[1].asWideString(), argIn[2].asWideStringVector(), argIn[3].asWideString());
        } else {
            throw std::runtime_error("Unknown application archive build operation.");
        }
    } catch (const std::exception& error) {
        RaiseError(error.what(), "Nelson:nelsonc:archive");
    }
    return {};
}
//=============================================================================
} // namespace Nelson::CompilerGateway
//=============================================================================
