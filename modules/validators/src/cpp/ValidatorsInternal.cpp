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
#include <boost/format.hpp>
#include "ValidatorsInternal.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "isfiniteBuiltin.hpp"
#include "allBuiltin.hpp"
#include "isemptyBuiltin.hpp"
#include "isscalarBuiltin.hpp"
#include "IsValidVariableName.hpp"
#include "isdirBuiltin.hpp"
#include "isvectorBuiltin.hpp"
#include "isfloatBuiltin.hpp"
#include "isnumericBuiltin.hpp"
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
static std::wstring
invalidPositionMessage(int argPosition)
{
    std::wstring msg;
    if (argPosition > 0) {
        msg = str(boost::wformat{ _W("Invalid input argument at position %d.") } % argPosition)
            + L"\n";
    }
    return msg;
}
//=============================================================================
void
mustBeLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
        if (!isLogical) {
            std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be logical.");
            std::wstring id = _W("Nelson:validators:mustBeLogical");
            Error(msg, id, asCaller);
        }
    }
}
//=============================================================================
void
mustBeFinite(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool asLogicalIsAllFinite = false;
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isfiniteBuiltin(_eval, 1, arg);
    argOut = ElementaryFunctionsGateway::allBuiltin(_eval, 1, argOut);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be finite.");
        std::wstring id = _W("Nelson:validators:mustBeFinite");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeLogicalScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (isLogical) {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
        if (argOut[0].getContentAsLogicalScalar()) {
            return;
        }
    }
    std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be logical scalar.");
    std::wstring id = _W("Nelson:validators:mustBeLogicalScalar");
    Error(msg, id, asCaller);
}
//=============================================================================
void
mustBeScalarOrEmpty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
        if (!argOut[0].getContentAsLogicalScalar()) {
            std::wstring msg
                = invalidPositionMessage(argPosition) + _W("Value must be scalar or empty.");
            std::wstring id = _W("Nelson:validators:mustBeScalarOrEmpty");
            Error(msg, id, asCaller);
        }
    }
}
//=============================================================================
void
mustBeValidVariableName(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isvarname = false;
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        isvarname = IsValidVariableName(arg.getContentAsWideString());
    }
    if (!isvarname) {
        std::wstring msg
            = invalidPositionMessage(argPosition) + _W("Value must be valid variable name.");
        std::wstring id = _W("Nelson:validators:mustBeValidVariableName");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeTextScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isTextScalar = arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
    if (!isTextScalar) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be a character vector or string scalar.");
        std::wstring id = _W("Nelson:validators:mustBeTextScalar");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeFolder(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    mustBeTextScalar(arg, argPosition, asCaller);
    ArrayOfVector argOut = FilesFoldersGateway::isdirBuiltin(1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be folder.");
        std::wstring id = _W("Nelson:validators:mustBeFolder");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeVector(const ArrayOf& arg, bool allowsAllEmpties, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isvectorBuiltin(_eval, 1, argIn);
    bool isVectorOrEmpty = argOut[0].getContentAsLogicalScalar();

    if (!isVectorOrEmpty && allowsAllEmpties) {
        argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
        isVectorOrEmpty = argOut[0].getContentAsLogicalScalar();
    }
    if (!isVectorOrEmpty) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a vector.");
        std::wstring id = _W("Nelson:validators:mustBeVector");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeFloat(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isfloatBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a float.");
        std::wstring id = _W("Nelson:validators:mustBeFloat");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNumeric(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be numeric.");
        std::wstring id = _W("Nelson:validators:mustBeNumeric");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
