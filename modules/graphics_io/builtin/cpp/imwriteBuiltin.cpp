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
#include "ErrorID.hpp"
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
    ArrayOf map = ArrayOf::empty(0, 0);
    ArrayOf alphaMap = ArrayOf::empty(0, 0);
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
        filename = argIn[2].asWideString();
        pos = 3;
    } else {
        filename = argIn[1].asWideString();
        pos = 2;
    }

    if ((argIn.size() - pos) > 0) {
        if ((argIn.size() - pos) > 1) {
            if ((argIn.size() - pos) % 2 != 0) {
                format = argIn[pos].asWideString();
                withExplicitFormat = true;
                pos = pos + 1;
            }
            for (indexType k = pos; k < argIn.size(); k = k + 2) {
                std::wstring name = argIn[k].asWideString();
                if (StringHelpers::iequals(name, L"LoopCount")) {
                    double value = argIn[k + 1].asDoubleScalar();
                    if (std::isinf(value)) {
                        loopCount = DEFAULT_LOOPCOUNT;
                    } else if (value == 0 || value == 1) {
                        loopCount = (int)(value + 1);
                    } else {
                        loopCount = (int)round(value);
                    }
                } else if (StringHelpers::iequals(name, L"WriteMode")) {
                    std::wstring value = argIn[k + 1].asWideString();
                    if (StringHelpers::iequals(value, L"overwrite")) {
                        append = false;
                    } else if (StringHelpers::iequals(value, L"append")) {
                        append = true;
                    } else {
                        Error(L"Nelson:imwrite:invalidWriteMode", L"%s expected.",
                            L"Wrong value for WriteMode: 'overwrite' or 'append'");
                    }
                } else if (StringHelpers::iequals(name, L"DelayTime")) {
                    double delayDouble = argIn[k + 1].asDoubleScalar();
                    if (delayDouble < 0 || delayDouble > 655) {
                        Error(L"Nelson:imwrite:invalidDelayTime",
                            L"Wrong value for delay time: [0, 655] expected.");
                    }
                    delayTime = static_cast<int>(std::lround(delayDouble * 1000.0));
                } else if (StringHelpers::iequals(name, L"quality")) {
                    quality = argIn[k + 1].asInt8Scalar();
                    if (quality < 0 || quality > 100) {
                        Error(L"Nelson:imwrite:invalidQuality",
                            L"Wrong value for quality: [0, 100] expected.");
                    }
                } else if (StringHelpers::iequals(name, L"alpha")) {
                    alphaMap = argIn[k + 1];
                } else {
                    wstringVector value = argIn[k + 1].asWideStringVector();
                    nameValue.insert(std::make_pair(name, value));
                }
            }
        } else {
            format = argIn[pos].asWideString();
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
            Error(L"Nelson:imwrite:unknownFormat",
                L"Unable to determine the file format from the file name.");
        }
    }

    bool hasAlpha = !alphaMap.isEmpty(true);
    bool hasColorMap = !map.isEmpty(true);
    Dimensions imageDimensions = image.dimensions();
    const bool grayOrIndexed = imageDimensions.length() == 2;
    const bool directColor
        = imageDimensions.length() == 3 && imageDimensions.dimensionLength(2) == 3;
    if ((!grayOrIndexed && !directColor) || image.elementCount() == 0) {
        Error(L"Nelson:imwrite:invalidImageDataShape", L"Image data must be either MxN or MxNx3.");
    }
    if (hasColorMap && !grayOrIndexed) {
        Error(L"Nelson:imwrite:colormapRequiresIndexed",
            L"A colormap can only be used with an MxN indexed image.");
    }
    if (hasColorMap) {
        Dimensions dimensionsMap = map.dimensions();
        bool checkDimensions = (dimensionsMap.length() == 2) && (dimensionsMap.columns() == 3);
        if (!checkDimensions) {
            Error(L"Nelson:imwrite:colormapColumns", L"Colormap should have three columns.");
        }
    }

    if (hasAlpha) {
        Dimensions dimensionsAlpha = alphaMap.dimensions();
        bool sameMN = (image.rows() == dimensionsAlpha.rows())
            && (image.columns() == dimensionsAlpha.columns());
        if (dimensionsAlpha.length() != 2 || !sameMN) {
            Error(L"Nelson:imwrite:invalidAlphaMap", L"Wrong size for AlphaMap.");
        }
    }

    bool isSupportedImageType = image.isDoubleClass() || image.isSingleClass() || image.isLogical()
        || image.dataClass() == NLS_UINT8 || image.dataClass() == NLS_UINT16;
    if (!isSupportedImageType) {
        Error(L"Nelson:imwrite:invalidImageType",
            L"Image must be double, single, logical, uint8 or uint16 type.");
    }

    if (image.dataClass() == NLS_UINT16
        && (!grayOrIndexed || hasColorMap
            || !(StringHelpers::iequals(format, L"png") || StringHelpers::iequals(format, L"tiff")
                || StringHelpers::iequals(format, L"tif")
                || StringHelpers::iequals(format, L"pgm")))) {
        Error(L"Nelson:imwrite:unsupportedFormat",
            L"uint16 images are only writable as PNG, TIFF or PGM.");
    }
    if (StringHelpers::iequals(format, L"tiff") || StringHelpers::iequals(format, L"tif")) {
        writeTiff(filename, image, map, alphaMap, nameValue);
        return retval;
    }
    if (StringHelpers::iequals(format, L"gif")) {
        writeGif(filename, append, image, map, alphaMap, quality, delayTime, loopCount, nameValue);
        return retval;
    }
    if (StringHelpers::iequals(format, L"pcx")) {
        writePcx(filename, image, map, alphaMap, quality, nameValue);
        return retval;
    }
    writeImage(filename, image, map, format, alphaMap, quality, nameValue);
    return retval;
}
//=============================================================================
