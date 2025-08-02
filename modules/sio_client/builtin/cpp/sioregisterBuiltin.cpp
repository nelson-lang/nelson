//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sioregisterBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "SioClientRegister.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SioClientGateway::sioregisterBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    switch (argIn.size()) {
    case 0: {
        nargoutcheck(nLhs, 0, 1);
        stringVector list = sioregisterList();
        Dimensions dims(list.size(), 1);
        retval << ArrayOf::stringArrayConstructor(list, dims);
    } break;
    case 2: {
        nargoutcheck(nLhs, 0, 0);
        ArrayOf param1 = argIn[0];
        std::string event_name = param1.getContentAsCString();
        ArrayOf param2 = argIn[1];
        std::string function_name = param2.getContentAsCString();
        if (issioregistered(event_name)) {
            Error(_("name already register."));
        }
        if (issioreserved(event_name)) {
            Error(_("reserved name"));
        }
        sioregister(event_name, function_name);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
