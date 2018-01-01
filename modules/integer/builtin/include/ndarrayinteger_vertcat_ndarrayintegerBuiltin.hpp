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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
    namespace IntegerGateway {
        ArrayOfVector ndarrayint8_vertcat_ndarrayint8Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector ndarrayint16_vertcat_ndarrayint16Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector ndarrayint32_vertcat_ndarrayint32Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector ndarrayint64_vertcat_ndarrayint64Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector ndarrayuint8_vertcat_ndarrayuint8Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector ndarrayuint16_vertcat_ndarrayuint16Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector ndarrayuint32_vertcat_ndarrayuint32Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector ndarrayuint64_vertcat_ndarrayuint64Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
    }
}
//=============================================================================
