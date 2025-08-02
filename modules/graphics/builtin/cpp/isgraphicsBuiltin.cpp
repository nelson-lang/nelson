//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isgraphicsBuiltin.hpp"
#include "GOPropertyNames.hpp"
#include "GraphicsObject.hpp"
#include "GORoot.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::isgraphicsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval = {};
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);

    Dimensions dims = argIn[0].getDimensions();

    if (argIn[0].getDataClass() != NLS_GO_HANDLE) {
        logical* ptrResult = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dims.getElementCount(), stringVector(), true);
        retval << ArrayOf(NLS_LOGICAL, dims, ptrResult);
        return retval;
    }
    std::wstring typeStr;
    if (argIn.size() == 2) {
        typeStr = argIn[1].getContentAsWideString();
        StringHelpers::to_lower(typeStr);
    }

    logical* ptrResult = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dims.getElementCount());
    ArrayOf results = ArrayOf(NLS_LOGICAL, dims, ptrResult);
    int64* handles = (int64*)argIn[0].getDataPointer();
    for (size_t k = 0; k < dims.getElementCount(); k++) {
        bool result = false;
        if (!isDeletedGraphicsObject(handles[k])) {
            GraphicsObject* fp = nullptr;
            try {
                if (handles[k] == HANDLE_ROOT_OBJECT) {
                    fp = getGraphicsRootObject();
                } else if (handles[k] >= HANDLE_OFFSET_OBJECT) {
                    fp = findGraphicsObject(handles[k], false);
                } else {
                    fp = (GraphicsObject*)findGOFigure(handles[k]);
                }
                if (!typeStr.empty() && fp) {
                    GOGenericProperty* hp = fp->findProperty(GO_TYPE_PROPERTY_NAME_STR, false);
                    if (hp) {
                        std::wstring r = hp->get().getContentAsWideString();
                        StringHelpers::to_lower(r);
                        result = (r == typeStr);
                    }
                } else {
                    result = (fp != nullptr);
                }
            } catch (const Exception&) {
                result = false;
            }
        }
        ptrResult[k] = result;
    }
    retval << results;
    return retval;
}
//=============================================================================
