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
// __xmlgenerateimages__(xmlFileOrDir, outputDir)
ArrayOfVector
Nelson::HelpToolsGateway::__xmlgenerateimages__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    std::wstring errorMessage;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 3);

    std::wstring xmlFileOrDir = argIn[0].getContentAsWideString();
    std::wstring outputDir = argIn[1].getContentAsWideString();
    wstringVector outputScriptFilenames;
    bool res = xmlDocGenerateImages(xmlFileOrDir, outputDir, outputScriptFilenames, errorMessage);

    retval << ArrayOf::logicalConstructor(res);
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(outputScriptFilenames);
    retval << ArrayOf::characterArrayConstructor(errorMessage);

    return retval;
}
//=============================================================================
