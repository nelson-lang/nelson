//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__xmldocgenerateimages__Builtin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "XmlDocGenerateImages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __xmlgenerateimages__(xmlFileOrDir, outputDir, force)
ArrayOfVector
Nelson::HelpToolsGateway::__xmlgenerateimages__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    std::wstring errorMessage;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 3);

    std::wstring xmlFileOrDir = argIn[0].asWideString();
    std::wstring outputDir = argIn[1].asWideString();
    bool force = false;
    if (argIn.size() == 3) {
        force = argIn[2].asLogicalScalar();
    }
    wstringVector outputScriptFilenames;
    bool res
        = xmlDocGenerateImages(xmlFileOrDir, outputDir, outputScriptFilenames, errorMessage, force);

    retval << ArrayOf::logicalScalar(res);
    retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(outputScriptFilenames);
    retval << ArrayOf::characterArray(errorMessage);

    return retval;
}
//=============================================================================
