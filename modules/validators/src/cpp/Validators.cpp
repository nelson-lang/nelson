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
#include "Validators.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "isfiniteBuiltin.hpp"
#include "allBuiltin.hpp"
#include "isemptyBuiltin.hpp"
#include "isscalarBuiltin.hpp"
#include "IsValidVariableName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Evaluator* _eval = nullptr;
//=============================================================================
void
setEvaluator(Evaluator* eval)
{
    _eval = eval;
}
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFinite(const ArrayOf& arg, bool asCaller)
{
    bool asLogicalIsAllFinite = false;
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isfiniteBuiltin(_eval, 1, arg);
    argOut = ElementaryFunctionsGateway::allBuiltin(_eval, 1, argOut);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = _W("Value must be finite.");
        std::wstring id = _W("Nelson:validators:mustBeFinite");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeLogical(const ArrayOf& arg, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
        if (!isLogical) {
            std::wstring msg = _W("Value must be logical.");
            std::wstring id = _W("Nelson:validators:mustBeLogical");
            Error(msg, id, asCaller);
        }
    }
}
//=============================================================================
void
mustBeLogicalScalar(const ArrayOf& arg, bool asCaller)
{
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (isLogical) {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
        if (argOut[0].getContentAsLogicalScalar()) {
            return;
        }
    }
    std::wstring msg = _W("Value must be logical scalar.");
    std::wstring id = _W("Nelson:validators:mustBeLogicalScalar");
    Error(msg, id, asCaller);
}
//=============================================================================
void
mustBeScalarOrEmpty(const ArrayOf& arg, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
        if (!argOut[0].getContentAsLogicalScalar()) {
            std::wstring msg = _W("Value must be scalar or empty.");
            std::wstring id = _W("Nelson:validators:mustBeScalarOrEmpty");
            Error(msg, id, asCaller);
        }
    }
}
//=============================================================================
void
mustBeValidVariableName(const ArrayOf& arg, bool asCaller)
{
    bool isvarname = false;
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        isvarname = IsValidVariableName(arg.getContentAsWideString());
    }
    if (!isvarname) {
        std::wstring msg = _W("Value must be valid variable name.");
        std::wstring id = _W("Nelson:validators:mustBeValidVariableName");
        Error(msg, id, asCaller);

    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
