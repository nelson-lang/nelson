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
#include "HorzCatOperator.hpp"
#include "HorzCat.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
hasStringArray(const ArrayOfVector& v)
{
    for (const auto& k : v) {
        if (k.isStringArray()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
hasCell(const ArrayOfVector& v)
{
    for (const auto& k : v) {
        if (k.isCell()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOf
HorzCatOperator(Evaluator* eval, const ArrayOfVector& v)
{
    ArrayOf res;
    switch (v.size()) {
    case 0: {
        res = ArrayOf::emptyConstructor();
    } break;
    case 1: {
        res = v[0];
    } break;
    default: {
        bool asCell = hasCell(v);
        bool asStringArray = hasStringArray(v);
        if (asStringArray) {
            bool needToOverload;
            if (!v[0].isStringArray()) {
                res = ArrayOf::toStringArray(v[0], needToOverload);
                if (needToOverload) {
                    Error(_("Conversion not possible."));
                }
            } else {
                res = v[0];
                res.ensureSingleOwner();
            }
            for (size_t k = 1; k < v.size(); k++) {
                ArrayOf arg2 = v[k];
                if ((!arg2.isStringArray())) {
                    arg2 = ArrayOf::toStringArray(arg2, needToOverload);
                    if (needToOverload) {
                        Error(_("Conversion not possible."));
                    }
                }
                res = eval->doBinaryOperatorOverload(res, arg2, HorzCat, "horzcat");
            }
            return res;
        }
        if (asCell) {
            if (!v[0].isCell() && !v[0].isEmpty()) {
                res = ArrayOf::toCell(v[0]);
            } else {
                res = v[0];
                res.ensureSingleOwner();
            }
            for (size_t k = 1; k < v.size(); k++) {
                ArrayOf arg2 = v[k];
                if ((!arg2.isCell()) && !arg2.isEmpty()) {
                    arg2 = ArrayOf::toCell(arg2);
                }
                res = eval->doBinaryOperatorOverload(res, arg2, HorzCat, "horzcat");
            }
            return res;
        }

        res = v[0];
        res.ensureSingleOwner();
        for (size_t k = 1; k < v.size(); k++) {
            ArrayOf arg2 = v[k];
            res = eval->doBinaryOperatorOverload(res, arg2, HorzCat, "horzcat");
        }
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
