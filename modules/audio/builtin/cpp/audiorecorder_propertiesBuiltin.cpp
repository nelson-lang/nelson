//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_propertiesBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    auto* objRec = (AudiorecorderObject*)param1.getContentAsHandleScalar();
    wstringVector props = objRec->getProperties();
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(props);
    return retval;
}
//=============================================================================
