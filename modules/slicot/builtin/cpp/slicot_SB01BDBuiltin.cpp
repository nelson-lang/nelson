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
#include "slicot_SB01BDBuiltin.hpp"
#include "slicot_SB01BD.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::SlicotGateway::slicot_SB01BDBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    std::string DICO = param1.getContentAsCString();
    ArrayOf param2 = argIn[1];
    double ALPHA = param2.getContentAsDoubleScalar();
    ArrayOf A = argIn[2];
    ArrayOf B = argIn[3];
    ArrayOf WR = argIn[4];
    ArrayOf WI = argIn[5];
    ArrayOf param7 = argIn[6];
    double TOL = param7.getContentAsDoubleScalar();
    retval = slicot_SB01BD(DICO, ALPHA, A, B, WR, WI, TOL);
    return retval;
}
//=============================================================================
