//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include "imwriteBuiltin.hpp"
#include "ImageWriter.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsIoGateway::imwriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // imwrite(A, filename)
    // imwrite(A, map, filename)
    // imwrite(..., filename, fmt)
    // imwrite(..., filename, fmt, "Comments", comment, "Quality", value)
    ArrayOfVector retval = {};
    nargincheck(argIn, 2, 10);
    nargoutcheck(nLhs, 0, 0);

    ArrayOf image = argIn[0];
    ArrayOf map = ArrayOf::emptyConstructor(0, 0);
    ArrayOf alphaMap = ArrayOf::emptyConstructor(0, 0);
    std::wstring filename;
    std::wstring format;
    bool withExplicitFormat = false;
    int loopCount = DEFAULT_LOOPCOUNT;
    bool append = false;
    int quality = DEFAULT_QUALITY;
    int delayTime = DEFAULT_DELAY;
    std::map<std::wstring, wstringVector> nameValue;

    indexType pos;

    if (argIn[1].isNumeric()) {
        map = argIn[1];
        nargincheck(argIn, 3, 10);
        filename = argIn[2].getContentAsWideString();
        pos = 3;
    } else {
        filename = argIn[1].getContentAsWideString();
        pos = 2;
    }

    if ((argIn.size() - pos) > 0) {
        if ((argIn.size() - pos) > 1) {
            if ((argIn.size() - pos) % 2 != 0) {
                format = argIn[pos].getContentAsWideString();
                withExplicitFormat = true;
                pos = pos + 1;
            }
            for (indexType k = pos; k < argIn.size(); k = k + 2) {
                std::wstring name = argIn[k].getContentAsWideString();
                if (StringHelpers::iequals(name, L"LoopCount")) {
                    double value = argIn[k + 1].getContentAsDoubleScalar();
                    if (std::isinf(value)) {
                        loopCount = DEFAULT_LOOPCOUNT;
                    } else if (value == 0 || value == 1) {
                        loopCount = (int)(value + 1);
                    } else {
                        loopCount = (int)round(value);
                    }
                } else if (StringHelpers::iequals(name, L"WriteMode")) {
                    std::wstring value = argIn[k + 1].getContentAsWideString();
                    if (StringHelpers::iequals(value, L"overwrite")) {
                        append = false;
                    } else if (StringHelpers::iequals(value, L"append")) {
                        append = true;
                    } else {
                        Error(_W("Wrong value for WriteMode: 'overwrite' or 'append' expected."));
                    }
                } else if (StringHelpers::iequals(name, L"DelayTime")) {
                    double delayDouble = argIn[k + 1].getContentAsDoubleScalar();
                    if (delayDouble < 0 || delayDouble > 655) {
                        Error(_W("Wrong value for delay time: [0, 655] expected."));
                    }
                    delayTime = (int)(delayDouble * 100);
                } else if (StringHelpers::iequals(name, L"quality")) {
                    quality = argIn[k + 1].getContentAsInteger8Scalar();
                    if (quality < 0 || quality > 100) {
                        Error(_W("Wrong value for quality: [0, 100] expected."));
                    }
                } else if (StringHelpers::iequals(name, L"alpha")) {
                    alphaMap = argIn[k + 1];
                } else {
                    wstringVector value = argIn[k + 1].getContentAsWideStringVector();
                    nameValue.insert(std::make_pair(name, value));
                }
            }
        } else {
            format = argIn[pos].getContentAsWideString();
            withExplicitFormat = true;
        }
    }

    if (!withExplicitFormat) {
        FileSystemWrapper::Path pathFileName(filename);
        format = pathFileName.extension().wstring();
        if (format.length() > 1) {
            format.erase(0, 1);
        }
        if (format.empty()) {
            Error(_W("Unable to determine the file format from the file name."));
        }
    }

    bool hasAlpha = !alphaMap.isEmpty(true);
    bool hasColorMap = !map.isEmpty(true);
    if (hasColorMap) {
        Dimensions dimensionsMap = map.getDimensions();
        bool checkDimensions
            = (dimensionsMap.getLength() == 2) && (dimensionsMap.getColumns() == 3);
        if (!checkDimensions) {
            Error(_W("Colormap should have three columns."));
        }
    }

    if (hasAlpha) {
        Dimensions dimensionsAlpha = alphaMap.getDimensions();
        indexType lenDimensionsAlpha = dimensionsAlpha.getLength();
        bool sameMN = (image.getRows() == dimensionsAlpha.getRows())
            && (image.getColumns() == dimensionsAlpha.getColumns());
        bool hasWrongSize = false;
        if (lenDimensionsAlpha == 2) {
            hasWrongSize = false;
        } else if (lenDimensionsAlpha == 3) {
            if (dimensionsAlpha.getDimensionLength(3) != 3) {
                hasWrongSize = true;
            }
        } else {
            hasWrongSize = true;
        }
        if (hasWrongSize || !sameMN) {
            Error(_W("Wrong size for AlphaMap."));
        }
    }

    bool isSupportedImageType = image.isDoubleClass() || image.isSingleClass() || image.isLogical()
        || (image.getDataClass() == NLS_UINT8);
    if (!isSupportedImageType) {
        Error(_W("Image must be double, single, logical or uint8 type."));
    }

#if WITH_TIFF
    if (StringHelpers::iequals(format, L"tiff") || StringHelpers::iequals(format, L"tif")) {
        writeTiff(filename, image, map, alphaMap, nameValue);
        return retval;
    }
#endif
#if WITH_GIF
    if (StringHelpers::iequals(format, L"gif")) {
        writeGif(filename, append, image, map, alphaMap, quality, delayTime, loopCount, nameValue);
        return retval;
    }
#endif
    if (StringHelpers::iequals(format, L"pcx")) {
        writePcx(filename, image, map, alphaMap, quality, nameValue);
        return retval;
    }
    writeImage(filename, image, map, format, alphaMap, quality, nameValue);
    return retval;
}
//=============================================================================
