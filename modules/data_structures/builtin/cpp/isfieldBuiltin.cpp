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
#include "isfieldBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::isfieldBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isfield", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param1.isClassStruct() || param1.isHandle()) {
            retval = OverloadFunction(eval, nLhs, argIn, "isfield", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        if (param1.isStruct()) {
            if (param2.isRowVectorCharacterArray()) {
                stringVector fieldnames = param1.getFieldNames();
                std::string name = param2.getContentAsCString();
                bool res = false;
                for (auto& fieldname : fieldnames) {
                    if (fieldname.compare(name) == 0) {
                        res = true;
                    }
                }
                retval << ArrayOf::logicalConstructor(res);
            } else if (param2.isCell()) {
                stringVector fieldnames = param1.getFieldNames();
                Dimensions dims2 = param2.getDimensions();
                if (dims2.getElementCount() == 0) {
                    retval << ArrayOf::logicalConstructor(false);
                } else {
                    auto* elements = (ArrayOf*)(param2.getDataPointer());
                    logical* res = static_cast<logical*>(ArrayOf::allocateArrayOf(
                        NLS_LOGICAL, dims2.getElementCount(), stringVector(), false));
                    ompIndexType elementCount = dims2.getElementCount();
                    for (ompIndexType k = 0; k < elementCount; ++k) {
                        res[k] = false;
                        if (elements[k].isRowVectorCharacterArray()) {
                            std::string name = elements[k].getContentAsCString();
                            for (auto& fieldname : fieldnames) {
                                if (fieldname.compare(name) == 0) {
                                    res[k] = true;
                                }
                            }
                        }
                    }
                    retval << ArrayOf(NLS_LOGICAL, dims2, res);
                }
            } else {
                retval << ArrayOf::logicalConstructor(false);
            }
        } else {
            retval << ArrayOf::logicalConstructor(false);
        }
    }
    return retval;
}
//=============================================================================
