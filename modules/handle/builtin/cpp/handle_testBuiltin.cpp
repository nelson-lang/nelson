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
#include "handle_testBuiltin.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::HandleGateway::handle_testBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
	size_t N = 3;
	double *ptr = new double[N];
	static double q = 0;
	for (size_t k = 0; k < N; k++)
	{
		ptr[k] = q;
		q = q + 1;
	}

	HandleGenericObject *hobj = new HandleGenericObject(L"HL_DOUBLE", ptr);
	retval.push_back(ArrayOf::handleConstructor(hobj));
	return retval;
}
//=============================================================================
