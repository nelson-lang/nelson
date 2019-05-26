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
#include "trueBuiltin.hpp"
#include "Error.hpp"
#include "LogicalConstructors.hpp"
#include "StringFormat.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LogicalGateway::trueBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool bIsSparse = false;
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.empty()) {
        Dimensions dim(1, 1);
        retval.push_back(TrueConstructor(dim, bIsSparse));
    } else {
        Dimensions dim;
        indexType idxMax = argIn.size();
        if ((static_cast<double>(argIn.size()) - 2.) >= 0.) {
            indexType pos = argIn.size() - 2;
            if (argIn[pos].isRowVectorCharacterArray()) {
                std::wstring arg = argIn[pos].getContentAsWideString();
                if (arg.compare(L"like") == 0) {
                    ArrayOf arg = argIn[pos + 1];
                    if (arg.getDataClass() != NLS_LOGICAL) {
                        Error(_W("Input following \'like\' is not a logical array."));
                    }
                    bIsSparse = arg.isSparse();
                    idxMax = argIn.size() - 2;
                    if (idxMax == 0) {
                        dim[0] = 1;
                        dim[1] = 1;
                    }
                } else {
                    Error(StringFormat(ERROR_WRONG_ARGUMENT_X_VALUE.c_str(), pos + 1));
                }
            }
        }
        for (indexType k = 0; k < idxMax; k++) {
            ArrayOf arg = argIn[k];
            indexType idx = arg.getContentAsScalarIndex();
            dim[k] = idx;
        }
        if (idxMax == 1) {
            dim[1] = dim[0];
        }
        dim.simplify();
        retval.push_back(TrueConstructor(dim, bIsSparse));
    }
    return retval;
}
//=============================================================================
