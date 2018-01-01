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
        ArrayOfVector int8_vertcat_int8Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector int16_vertcat_int16Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector int32_vertcat_int32Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector int64_vertcat_int64Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector uint8_vertcat_uint8Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector uint16_vertcat_uint16Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector uint32_vertcat_uint32Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
        ArrayOfVector uint64_vertcat_uint64Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
    }
}
//=============================================================================
