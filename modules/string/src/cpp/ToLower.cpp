//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <cctype>
#include <string>
#include <algorithm>
#include "ToLower.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf ToLower(Evaluator* eval, ArrayOf A)
    {
        ArrayOf res;
        if (A.isSingleString())
        {
            return ArrayOf::stringConstructor(ToLower(A.getContentsAsWideString()));
        }
        else if (A.getDataClass() == NLS_CELL_ARRAY)
        {
            if (A.isEmpty())
            {
                return ArrayOf(A);
            }
            else
            {
                res = ArrayOf(A);
                res.ensureSingleOwner();
                ArrayOf *element = (ArrayOf*)(res.getDataPointer());
                for (indexType k = 0; k < A.getDimensions().getElementCount(); k++)
                {
                    if (!element[k].isSingleString())
                    {
                        Error(eval, ERROR_TYPE_CELL_OF_STRINGS_EXPECTED);
                    }
                    element[k] = ArrayOf::stringConstructor(ToLower(element[k].getContentsAsWideString()));
                }
                return res;
            }
        }
        else
        {
            Error(eval, ERROR_TYPE_CELL_OF_STRINGS_EXPECTED);
        }
        return res;
    }
    //=============================================================================
    std::wstring ToLower(std::wstring A)
    {
        std::wstring res = A;
        transform(res.begin(), res.end(), res.begin(), towlower);
        return res;
    }
    //=============================================================================
}
//=============================================================================
