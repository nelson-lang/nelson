//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "versionBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Nelson_VERSION.h"
#include "VersionCompilerFlags.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::versionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        nargoutcheck(nLhs, 0, 2);
        std::string version;
        version.append(NELSON_VERSION_STRING);
        version.append(" (");
        version.append(NELSON_RELEASE_NAME);
        version.append(")");
        retval << ArrayOf::characterArray(version);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(__DATE__);
        }
    } else if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 1);
        std::wstring option;
        if (argIn[0].isRowVectorCharacterArray()) {
            option = argIn[0].asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        if (option == L"-semantic") {
            retval << ArrayOf::characterArray(NELSON_SEMANTIC_VERSION_STRING);
        } else if (option == L"-date") {
            retval << ArrayOf::characterArray(std::string(__DATE__) + " " + std::string(__TIME__));
        } else if (option == L"-description") {
            retval << ArrayOf::characterArray(L"");
        } else if (option == L"-release") {
            retval << ArrayOf::characterArray(NELSON_RELEASE_NAME);
        } else if (option == L"-compiler") {
            retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(VersionCompilerFlags());
        } else if (option == L"-commit_hash") {
            retval << ArrayOf::characterArray(NELSON_VERSION_COMMIT_HASH);
        } else if (option == L"-number") {
            ArrayOf vectRes = ArrayOf::doubleRowVector(4);
            auto* vectAsDouble = static_cast<double*>(vectRes.readWriteDataPointer());
            if (vectAsDouble != nullptr) {
                vectAsDouble[0] = NELSON_VERSION_MAJOR;
                vectAsDouble[1] = NELSON_VERSION_MINOR;
                vectAsDouble[2] = NELSON_VERSION_MAINTENANCE;
                vectAsDouble[3] = NELSON_VERSION_BUILD;
            }
            retval << vectRes;
        } else {
            Error(L"Nelson:version:invalidOption", L"Unknown option.");
        }
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    return retval;
}
//=============================================================================
