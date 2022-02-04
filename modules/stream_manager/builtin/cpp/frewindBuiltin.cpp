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
#include "frewindBuiltin.hpp"
#include "Error.hpp"
#include "File.hpp"
#include "FileRewind.hpp"
#include "FilesManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::frewindBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    auto* fm = static_cast<FilesManager*>(eval->FileManager);
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    ArrayOf param1 = argIn[0];
    auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
    if (fm->isOpened(iValue)) { //-V1004
        File* f = fm->getFile(iValue);
        if (f->isInterfaceMethod()) {
            Error(_W("Rewind failed."));
        } else {
            FileRewind(f);
        }
    } else {
        Error(_W("Invalid file identifier."));
    }
    return retval;
}
//=============================================================================
