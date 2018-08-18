//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "CheckIJV.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
CheckIJV(
    size_t ilen, size_t jlen, size_t vlen, int& istride, int& jstride, int& vstride, size_t& olen)
{
    olen = 0;
    olen = ilen > jlen ? ilen : jlen;
    olen = vlen > olen ? vlen : olen;
    istride = 0;
    jstride = 0;
    vstride = 0;
    if (olen > 1) {
        if (ilen == 1) {
            istride = 0;
        } else if (ilen == olen) {
            istride = 1;
        } else {
            Error(_W("in I, J, V format, all three vectors must be the same size or be scalars."));
        }
        if (jlen == 1) {
            jstride = 0;
        } else if (jlen == olen) {
            jstride = 1;
        } else {
            Error(_W("in I, J, V format, all three vectors must be the same size or be scalars."));
        }
        if (vlen == 1) {
            vstride = 0;
        } else if (vlen == olen) {
            vstride = 1;
        } else {
            Error(_W("in I, J, V format, all three vectors must be the same size or be scalars."));
        }
    }
    return true;
}
//=============================================================================
}
//=============================================================================
