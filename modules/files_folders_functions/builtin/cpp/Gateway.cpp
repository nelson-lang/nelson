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
#include "NelsonGateway.hpp"
#include "cdBuiltin.hpp"
#include "copyfileBuiltin.hpp"
#include "diff_fileBuiltin.hpp"
#include "dirBuiltin.hpp"
#include "filepartsBuiltin.hpp"
#include "filesepBuiltin.hpp"
#include "isdirBuiltin.hpp"
#include "isfileBuiltin.hpp"
#include "mkdirBuiltin.hpp"
#include "pathsepBuiltin.hpp"
#include "pwdBuiltin.hpp"
#include "relativepathBuiltin.hpp"
#include "rmdirBuiltin.hpp"
#include "rmfileBuiltin.hpp"
#include "tempdirBuiltin.hpp"
#include "userdirBuiltin.hpp"
#include "fullpathBuiltin.hpp"
#include "fullfileBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"files_folders_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "pwd", (ptrBuiltin)Nelson::FilesFoldersGateway::pwdBuiltin, 1, 0 },
    { "dir", (ptrBuiltin)Nelson::FilesFoldersGateway::dirBuiltin, 1, 1 },
    { "cd", (ptrBuiltin)Nelson::FilesFoldersGateway::cdBuiltin, 1, 1 },
    { "isdir", (ptrBuiltin)Nelson::FilesFoldersGateway::isdirBuiltin, 1, 1 },
    { "isfolder", (ptrBuiltin)Nelson::FilesFoldersGateway::isdirBuiltin, 1, 1 },
    { "isfile", (ptrBuiltin)Nelson::FilesFoldersGateway::isfileBuiltin, 1, 1 },
    { "fileparts", (ptrBuiltin)Nelson::FilesFoldersGateway::filepartsBuiltin, 3, 2 },
    { "mkdir", (ptrBuiltin)Nelson::FilesFoldersGateway::mkdirBuiltin, 2, 2 },
    { "relativepath", (ptrBuiltin)Nelson::FilesFoldersGateway::relativepathBuiltin, 1, 2 },
    { "rmdir", (ptrBuiltin)Nelson::FilesFoldersGateway::rmdirBuiltin, 2, 2 },
    { "copyfile", (ptrBuiltin)Nelson::FilesFoldersGateway::copyfileBuiltin, 2, 3 },
    { "diff_file", (ptrBuiltin)Nelson::FilesFoldersGateway::diff_fileBuiltin, 1, -2 },
    { "rmfile", (ptrBuiltin)Nelson::FilesFoldersGateway::rmfileBuiltin, 2, 1 },
    { "pathsep", (ptrBuiltin)Nelson::FilesFoldersGateway::pathsepBuiltin, 1, 0 },
    { "filesep", (ptrBuiltin)Nelson::FilesFoldersGateway::filesepBuiltin, 1, 0 },
    { "fullpath", (ptrBuiltin)Nelson::FilesFoldersGateway::fullpathBuiltin, 1, 1 },
    { "tempdir", (ptrBuiltin)Nelson::FilesFoldersGateway::tempdirBuiltin, 1, 0 },
    { "userdir", (ptrBuiltin)Nelson::FilesFoldersGateway::userdirBuiltin, 1, 0 },
    { "fullfile", (ptrBuiltin)Nelson::FilesFoldersGateway::fullfileBuiltin, 1, -1 },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
