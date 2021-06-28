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
#include "conv2Builtin.hpp"
#include "Convolution2D.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::conv2Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "conv2", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res;
        switch (argIn.size()) {
        case 2: {
            res = Convolution2D(argIn[0], argIn[1], L"full", needToOverload);
        } break;
        case 3: {
            if ((argIn[2].isRowVectorCharacterArray()
                    || (argIn[2].isStringArray() && argIn[2].isScalar()))) {
                std::wstring shape = argIn[2].getContentAsWideString();
                res = Convolution2D(argIn[0], argIn[1], shape, needToOverload);
            } else {
                res = Convolution2D(argIn[0], argIn[1], argIn[2], L"full", needToOverload);
            }
        } break;
        case 4: {
            std::wstring shape = argIn[3].getContentAsWideString();
            res = Convolution2D(argIn[0], argIn[1], argIn[2], shape, needToOverload);
        } break;
        default: {
            needToOverload = true;
        } break;
        }
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "conv2");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
