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
#include "Cast.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Cast(ArrayOf arrayIn, Class destinationClass, bool isSparse)
{
    ArrayOf res;
    res = arrayIn;
    Class originClass = res.getDataClass();
    if (originClass == NLS_SCOMPLEX || originClass == NLS_DCOMPLEX) {
        if (destinationClass == NLS_DOUBLE) {
            destinationClass = NLS_DCOMPLEX;
        }
        if (destinationClass == NLS_SINGLE) {
            destinationClass = NLS_SCOMPLEX;
        }
    }
    if ((originClass == NLS_SCOMPLEX || originClass == NLS_DCOMPLEX)
        && (destinationClass != NLS_SCOMPLEX && destinationClass != NLS_DCOMPLEX)) {
        Error(_W("Invalid conversion from complex."));
    }
    res.promoteType(destinationClass);
    if (isSparse) {
        res.makeSparse();
    }
    return res;
}
//=============================================================================
}
//=============================================================================
