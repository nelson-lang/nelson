//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "userpathBuiltin.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::userpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 0);
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring paramstr = param1.getContentAsWideString();
            if (paramstr == L"clear") {
                PathFuncManager::getInstance()->clearUserPath(true);
            } else if (paramstr == L"reset") {
                PathFuncManager::getInstance()->resetUserPath();
            } else {
                boost::filesystem::path data_dir(paramstr);
                bool bRes = false;
                try {
                    bRes = boost::filesystem::is_directory(data_dir);
                } catch (const boost::filesystem::filesystem_error& e) {
                    if (e.code() == boost::system::errc::permission_denied) {
                        // ONLY FOR DEBUG
                    }
                    bRes = false;
                }
                if (bRes) {
                    PathFuncManager::getInstance()->setUserPath(paramstr, true);
                } else {
                    Error(_W("Not an existing directory:") + L" " + paramstr);
                }
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        nargincheck(argIn, 0, 0);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArrayConstructor(PathFuncManager::getInstance()->getUserPath());
    }
    return retval;
}
//=============================================================================
