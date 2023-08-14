//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "overloadmodeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OverloadGateway::overloadmodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);

    OverloadLevelCompatibility previousOverloadMode
        = NelsonConfiguration::getInstance()->getOverloadLevelCompatibility();

    if (argIn.size() == 1) {
        std::string param = argIn[0].getContentAsCString();
        if (param == "none") {
            NelsonConfiguration::getInstance()->setOverloadLevelCompatibility(
                OverloadLevelCompatibility::NLS_OVERLOAD_NONE);
        } else if (param == "object") {
            NelsonConfiguration::getInstance()->setOverloadLevelCompatibility(
                OverloadLevelCompatibility::NLS_OVERLOAD_OBJECT_TYPES_ONLY);
        } else if (param == "all") {
            NelsonConfiguration::getInstance()->setOverloadLevelCompatibility(
                OverloadLevelCompatibility::NLS_OVERLOAD_ALL_TYPES);
        } else {
            Error(_W("Invalid argument: 'none', 'object', 'all' expected."));
        }
    }
    if (nLhs > 0) {
        switch (previousOverloadMode) {
        case OverloadLevelCompatibility::NLS_OVERLOAD_NONE: {
            retval << ArrayOf::characterArrayConstructor("none");
        } break;
        case OverloadLevelCompatibility::NLS_OVERLOAD_ALL_TYPES: {
            retval << ArrayOf::characterArrayConstructor("all");
        } break;
        case OverloadLevelCompatibility::NLS_OVERLOAD_OBJECT_TYPES_ONLY: {
            retval << ArrayOf::characterArrayConstructor("object");
        } break;
        }
    }
    return retval;
}
//=============================================================================
