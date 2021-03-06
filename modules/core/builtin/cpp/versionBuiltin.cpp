//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "versionBuiltin.hpp"
#include "Error.hpp"
#include "Nelson_VERSION.h"
#include "ToCellString.hpp"
#include "VersionCompilerFlags.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::versionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        if (nLhs > 2) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::string version;
        version.append(NELSON_VERSION_STRING);
        version.append(" (");
        version.append(NELSON_RELEASE_NAME);
        version.append(")");
        retval << ArrayOf::characterArrayConstructor(version);
        if (nLhs > 1) {
            retval << ArrayOf::characterArrayConstructor(__DATE__);
        }
    } else if (argIn.size() == 1) {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring option;
        if (argIn[0].isRowVectorCharacterArray()) {
            option = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        if (option == L"-date") {
            retval << ArrayOf::characterArrayConstructor(__TIMESTAMP__);
        } else if (option == L"-description") {
            retval << ArrayOf::characterArrayConstructor(L"");
        } else if (option == L"-release") {
            retval << ArrayOf::characterArrayConstructor(NELSON_RELEASE_NAME);
        } else if (option == L"-compiler") {
            retval << ToCellStringAsRow(VersionCompilerFlags());
        } else if (option == L"-commit_hash") {
            retval << ArrayOf::characterArrayConstructor(NELSON_VERSION_COMMIT_HASH);
        } else if (option == L"-number") {
            ArrayOf vectRes = ArrayOf::doubleVectorConstructor(4);
            auto* vectAsDouble = static_cast<double*>(vectRes.getReadWriteDataPointer());
            if (vectAsDouble != nullptr) {
                vectAsDouble[0] = NELSON_VERSION_MAJOR;
                vectAsDouble[1] = NELSON_VERSION_MINOR;
                vectAsDouble[2] = NELSON_VERSION_MAINTENANCE;
                vectAsDouble[3] = NELSON_VERSION_BUILD;
            }
            retval << vectRes;
        } else {
            Error(_W("Unknow option."));
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
