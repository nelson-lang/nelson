//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lengthBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::lengthBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    double len = 0;
    Dimensions sze(param1.getDimensions());
    for (indexType i = 0; i < sze.getLength(); i++) {
        if (static_cast<double>(sze[i]) == 0) {
            len = 0;
            break;
        }
        if (i == 0) {
            len = static_cast<double>(sze[i]);
        } else {
            if (static_cast<double>(sze[i]) > len) {
                len = static_cast<double>(sze[i]);
            }
        }
    }
    retval << ArrayOf::doubleConstructor(len);
    return retval;
}
//=============================================================================
