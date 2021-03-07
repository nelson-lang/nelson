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
#include "audioplayer_ismethodBuiltin.hpp"
#include "AudioplayerObject.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_ismethodBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != AUDIOPLAYER_CATEGORY_STR) {
        Error(_W("audioplayer handle expected."));
    }
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    retval << ArrayOf::logicalConstructor(param1.isHandleMethod(propertyName));
    return retval;
}
//=============================================================================
