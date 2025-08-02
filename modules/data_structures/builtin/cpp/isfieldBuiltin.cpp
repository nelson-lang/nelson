//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isfieldBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::isfieldBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (param1.isClassType() || param1.isHandle()) {
        OverloadRequired("isfield");
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
    return retval;
}
//=============================================================================
