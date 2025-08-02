//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dllibinfoBuiltin.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dllibinfoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_DLLIB_CATEGORY_STR) {
        Error(_W("dllib handle expected."));
    }
    auto* obj = (DynamicLinkLibraryObject*)param1.getContentAsHandleScalar();
    std::string errorMessage;
    stringVector symbols = obj->getAvailableSymbols(errorMessage);
    if (!errorMessage.empty()) {
        Error(_("Cannot get library symbols: ") + errorMessage);
    }
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(symbols);
    return retval;
}
//=============================================================================
