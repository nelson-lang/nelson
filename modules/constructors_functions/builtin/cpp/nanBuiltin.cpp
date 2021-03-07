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
#include "nanBuiltin.hpp"
#include "Error.hpp"
#include "NaN.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::nanBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    uint32 m = 1;
    uint32 n = 1;
    ArrayOf p;
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    if (argIn.empty()) {
        m = 1;
        n = 1;
    } else {
        p = argIn[0];
        m = p.getContentAsInteger32Scalar();
        n = m;
    }
    if (argIn.size() > 1) {
        p = argIn[1];
        n = p.getContentAsInteger32Scalar();
    }
    retval << NaN(m, n);
    return retval;
}
//=============================================================================
