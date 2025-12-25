//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audioplayer_propertiesBuiltin.hpp"
#include "AudioplayerObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "NelsonPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR) {
        Error(_W("audioplayer handle expected."));
    }
    auto* objPlayer = (AudioplayerObject*)param1.getContentAsHandleScalar();
    wstringVector fieldnames = objPlayer->fieldnames();
    if (nLhs == 0) {
        std::wstring msg;
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
            msg = L"\n";
        }
        if (fieldnames.empty()) {
            msg = msg + _W("No property for class: audioplayer.") + L"\n";
        } else {
            msg = msg + _W("Properties for class: audioplayer:") + L"\n\n";
            for (auto& fieldname : fieldnames) {
                msg = msg + std::wstring(L"\t") + fieldname + std::wstring(L"\n");
            }
        }
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
            msg = msg + std::wstring(L"\n");
        }
        NelsonPrint(msg);
    } else {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(fieldnames);
    }
    return retval;
}
//=============================================================================
