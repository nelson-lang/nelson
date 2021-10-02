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
#include "inputnameBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::inputnameBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    Context* context = eval->getContext();
    if (context->getCurrentScope()->getName() == "base") {
        Error(_W("Cannot return input name if not in an active function."),
            L"Nelson:inputname:notInAfunction");
    }
    double pos = argIn[0].getContentAsDoubleScalar();
    int ipos = (int)pos;
    if ((double)ipos != pos) {
        Error(_W("Scalar integer value required, but value is not integral."), L"Nelson:IntVal");
    }
    int nargin = context->getCurrentScope()->getNargIn();
    if (ipos > nargin || ipos < 1) {
        Error(_W("Argument number is not valid."), L"Nelson:inputname:argNumberNotValid");
    }
    stringVector inputNames = context->getCurrentScope()->getInputArgumentNames();
    std::string name;
    ipos = ipos - 1;
    if (ipos < inputNames.size()) {
        name = inputNames[ipos];
    }
    retval << ArrayOf::characterArrayConstructor(name);
    return retval;
}
//=============================================================================
