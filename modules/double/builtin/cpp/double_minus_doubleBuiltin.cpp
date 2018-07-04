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
#include "double_minus_doubleBuiltin.hpp"
#include "Error.hpp"
#include "MinusDouble.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::double_minus_doubleBuiltin(Evaluator *eval, int nLhs,
                                                  const ArrayOfVector &argIn) {
  if (argIn.size() != 2) {
    Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
  }
  ArrayOf A = argIn[0];
  ArrayOf B = argIn[1];
  if (!A.isDoubleType() || !B.isDoubleType()) {
    Error(eval, ERROR_WRONG_ARGUMENTS_TYPE_DOUBLE_EXPECTED);
  }
  if (A.isSparse() || B.isSparse()) {
    Error(eval, ERROR_WRONG_ARGUMENTS_SIZE_FULL_MATRIX_EXPECTED);
  }
  if (!A.is2D() || !B.is2D()) {
    Error(eval, ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
  }
  ArrayOfVector retval;
  ArrayOf res = double_minus_double(A, B);
  retval.push_back(res);
  return retval;
}
//=============================================================================
