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
#include "Assert_IsApprox.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
Assert_IsApprox(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray,
    double precision, std::wstring& msg)
{
    bool bRes = false;
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string IsApproxName = "isapprox";
    if (context->lookupFunction(IsApproxName, funcDef)) {
        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
            ArrayOfVector argInCopy(3);
            argInCopy << computedArray;
            argInCopy << expectedArray;
            argInCopy << ArrayOf::doubleConstructor(precision);
            try {
                ArrayOfVector resVect = funcDef->evaluateFunction(eval, argInCopy, 1);
                if (resVect.size() != 1) {
                    Error(_W("isapprox returns more than one output argument."));
                }
                ArrayOf r = resVect[0];
                if (r.isScalar() && r.isLogical()) {
                    bRes = r.getContentAsLogicalScalar() ? true : false;
                } else {
                    Error(_W("isapprox must return an logical."));
                }
            } catch (const Exception&) {
                throw;
            }
        }
    } else {
        Error("isapprox function not found.");
    }
    if (!bRes) {
        msg = _W("Assertion failed: expected and computed values are too different.");
    } else {
        msg.clear();
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
