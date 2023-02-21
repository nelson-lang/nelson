//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isValidGraphicsPropertyBuiltin.hpp"
#include "GraphicsObject.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GORoot.hpp"
#include "GOImage.hpp"
#include "GOLineSeries.hpp"
#include "GOText.hpp"
#include "StringHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::isValidGraphicsPropertyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    std::wstring GOTypename = argIn[0].getContentAsWideString();
    std::wstring GOPropertyName = argIn[1].getContentAsWideString();

    GraphicsObject* fp = nullptr;
    if (StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_AXES_STR)) {
        fp = new GOAxis();
    }
    if (StringHelpers::iequals(GOTypename, L"line")) {
        fp = new GOLineSeries();
    }
    if (StringHelpers::iequals(GOTypename, L"image")) {
        fp = new GOImage();
    }
    if (StringHelpers::iequals(GOTypename, L"root")) {
        fp = new GORoot();
    }
    if (StringHelpers::iequals(GOTypename, L"text")) {
        fp = new GOText();
    }
    if (StringHelpers::iequals(GOTypename, L"figure")) {
        fp = new GOFigure(nullptr, -1);
    }

    if (!fp) {
        Error(_W("Invalid Graphics object type name:") + GOTypename);
    }

    std::vector<std::wstring> propertyNames = fp->getFieldnames();
    bool isValidPropertyName = false;
    for (auto name : propertyNames) {
        if (StringHelpers::iequals(name, GOPropertyName)) {
            isValidPropertyName = true;
            break;
        }
    }
    if (fp) {
        delete fp;
        fp = nullptr;
    }
    retval << ArrayOf::logicalConstructor(isValidPropertyName);
    return retval;
}
//=============================================================================
