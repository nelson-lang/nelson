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
#include "VertCatOperator.hpp"
#include "VertCat.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatOperator(Evaluator* eval, const ArrayOfVector& v)
{
    ArrayOf res;
    switch (v.size()) {
    case 0: {
        res = ArrayOf::emptyConstructor();
    } break;
    case 1: {
        res = v[0];
        res.ensureSingleOwner();
    } break;
    default: {
        res = v[0];
        res.ensureSingleOwner();
        for (size_t k = 1; k < v.size(); k++) {
            ArrayOf arg2 = v[k];
            res = eval->doBinaryOperatorOverload(res, arg2, VertCat, "vertcat");
        }
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
