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
#include "NotLogical.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf NotLogical(ArrayOf A)
    {
        ArrayOf C;
        if (A.getDataClass() == NLS_LOGICAL)
        {
            size_t Alen(A.getLength());
            logical *Cp = new_with_exception<logical>(Alen);
            C = ArrayOf(NLS_LOGICAL, A.getDimensions(), Cp);
            logical *Ap = (logical*)A.getDataPointer();
            for (size_t i = 0; i < Alen; i++)
            {
                if (Ap[i] == 0)
                {
                    Cp[i] = 1;
                }
                else
                {
                    Cp[i] = 0;
                }
            }
        }
        else
        {
            throw Exception(_W("Invalid type."));
        }
        return C;
    }
    //=============================================================================
}
//=============================================================================
