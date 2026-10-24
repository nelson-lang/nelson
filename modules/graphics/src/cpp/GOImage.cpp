//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <atomic>
#include <cmath>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOImage.hpp"
#include "GOAxis.hpp"
#include "GODataMappingModeProperty.hpp"
#include "GOMappingModeProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<double>
imageCoordinateSpan(const std::vector<double>& coordinates, indexType count);
//=============================================================================
static std::vector<double>
imageCoordinatesOrDefault(const std::vector<double>& coordinates, indexType count);
//=============================================================================
static void
setImageCoordinates(GOGenericProperty* property, const std::vector<double>& coordinates);
//=============================================================================
namespace {
    std::atomic<uint64_t> nextImageResourceKey { 1 };
    //=============================================================================
    unsigned char
    byteFromScaled(double value)
    {
        if (!std::isfinite(value)) {
            return 0;
        }
        int byte = static_cast<int>(value);
        byte = std::max(0, std::min(255, byte));
        return static_cast<unsigned char>(byte);
    }
    //=============================================================================
    unsigned char
    byteFromUnit(double value)
    {
        return byteFromScaled(255.0 * value);
    }
    //=============================================================================
    unsigned char
    alphaAt(const std::vector<double>* alpha, indexType idx)
    {
        return alpha ? byteFromUnit((*alpha)[idx]) : 255;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
GOImage::getType()
{
    return GO_PROPERTY_VALUE_IMAGE_STR;
}
//=============================================================================
GOImage::GOImage() : imageResourceKey(nextImageResourceKey.fetch_add(1))
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
void
GOImage::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_ALPHA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOMappingModeProperty, GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GODataMappingModeProperty, GO_C_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOImage::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR);
    std::vector<double> gp;
    gp.push_back(1.0);
    hp->data(gp);
    setRestrictedStringDefault(GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DIRECT_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
}
//=============================================================================
GOImage::~GOImage() { }
//=============================================================================
std::vector<double>
GOImage::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    limitsDirty = false;
    GOVectorProperty* xp = (GOVectorProperty*)findProperty(GO_X_DATA_PROPERTY_NAME_STR);
    GOVectorProperty* yp = (GOVectorProperty*)findProperty(GO_Y_DATA_PROPERTY_NAME_STR);

    limits.resize(0);
    limits.reserve(10);
    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    indexType rows = std::max<indexType>(1, cdata.rows());
    indexType cols = std::max<indexType>(1, cdata.columns());
    std::vector<double> XP(imageCoordinateSpan(imageCoordinatesOrDefault(xp->data(), cols), cols));
    std::vector<double> YP(imageCoordinateSpan(imageCoordinatesOrDefault(yp->data(), rows), rows));
    if (XP.size() < 2 || YP.size() < 2) {
        return limits;
    }
    double dx = (cols > 1) ? (XP[1] - XP[0]) / (double)(cols - 1) : 1.0;
    double dy = (rows > 1) ? (YP[1] - YP[0]) / (double)(rows - 1) : 1.0;
    double x1 = XP[0] - dx / 2.0;
    double x2 = XP[1] + dx / 2.0;
    double y1 = YP[0] - dy / 2.0;
    double y2 = YP[1] + dy / 2.0;
    limits.push_back(std::min(x1, x2));
    limits.push_back(std::max(x1, x2));
    limits.push_back(std::min(y1, y2));
    limits.push_back(std::max(y1, y2));
    limits.push_back(0);
    limits.push_back(0);

    if (!cdata.isEmpty()) {
        cdata.promoteType(NLS_DOUBLE);
        limits.push_back(ArrayOfMin(cdata));
        limits.push_back(ArrayOfMax(cdata));
    } else {
        limits.push_back(0);
        limits.push_back(1);
    }
    std::vector<double> alphadata(findVectorDoubleProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR));
    limits.push_back(findVectorMin(alphadata));
    limits.push_back(findVectorMax(alphadata));
    return limits;
}
//=============================================================================
void
GOImage::updateCAlphadata()
{
    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    if (cdata.isEmpty() || !(cdata.isNumeric() || cdata.isLogical())) {
        return;
    }
    const bool trueColor = (cdata.dimensions().length() == 3) && (cdata.dimensionLength(2) == 3);
    const bool uint8TrueColor = trueColor && cdata.dataClass() == NLS_UINT8;
    const bool cdata_is_integer = cdata.isIntegerType() || cdata.isLogical();
    std::vector<double> alphas;
    const std::vector<double>* alphaPtr = nullptr;
    if (!usesUnitAlpha()) {
        alphas = getAlphaMap(cdata.rows(), cdata.columns());
        alphaPtr = &alphas;
    }
    if (uint8TrueColor) {
        prepareTrueColorRGBA8(static_cast<const unsigned char*>(cdata.dataPointer()), cdata.rows(),
            cdata.columns(), alphaPtr);
    } else {
        cdata.promoteType(NLS_DOUBLE);
        const double* data = static_cast<const double*>(cdata.dataPointer());
        if (trueColor) {
            prepareTrueColorRGBA(data, cdata.rows(), cdata.columns(), alphaPtr, cdata_is_integer);
        } else {
            prepareMappedRGBA(data, cdata.rows(), cdata.columns(), alphaPtr, !cdata_is_integer);
        }
    }
    imageOpaque = alphaPtr == nullptr;
    if (!imageOpaque) {
        imageOpaque = true;
        for (size_t index = 3; index < imgRgba.size(); index += 4) {
            if (imgRgba[index] != 255) {
                imageOpaque = false;
                break;
            }
        }
    }
    ++imageRevision;
}
//=============================================================================
void
GOImage::updateState()
{
    GOAxis* ax = getParentAxis();
    GOFigure* fig = getParentFigure();
    bool updateImage = hasChanged(GO_C_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_C_DATA_MAPPING_PROPERTY_NAME_STR);
    if (ax) {
        updateImage = updateImage || ax->hasChanged(GO_C_LIM_PROPERTY_NAME_STR)
            || ax->hasChanged(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    if (fig) {
        updateImage = updateImage || fig->hasChanged(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    if (updateImage) {
        updateCAlphadata();
        limitsDirty = true;
    }
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR) || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
    }
    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    GOVectorProperty* xp = (GOVectorProperty*)findProperty(GO_X_DATA_PROPERTY_NAME_STR);
    if (xp->data().empty() && !cdata.isEmpty()) {
        if (cdata.columns() > 1) {
            setImageCoordinates(xp, { 1., static_cast<double>(cdata.columns()) });
        } else {
            setImageCoordinates(xp, { 1. });
        }
    }
    GOVectorProperty* yp = (GOVectorProperty*)findProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    if (yp->data().empty() && !cdata.isEmpty()) {
        if (cdata.rows() > 1) {
            setImageCoordinates(yp, { 1., static_cast<double>(cdata.rows()) });
        } else {
            setImageCoordinates(yp, { 1. });
        }
    }
    clearAllChanged();
}
//=============================================================================
void
GOImage::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOVectorProperty* xp = (GOVectorProperty*)findProperty(GO_X_DATA_PROPERTY_NAME_STR);
    GOVectorProperty* yp = (GOVectorProperty*)findProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    GOAxis* ax = getParentAxis();
    if (!ax || !xp || !yp) {
        return;
    }
    GOTwoVectorProperty* xLim = (GOTwoVectorProperty*)ax->findProperty(GO_X_LIM_PROPERTY_NAME_STR);
    GOTwoVectorProperty* yLim = (GOTwoVectorProperty*)ax->findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
    if (!xLim || !yLim) {
        return;
    }
    bool xFlip = (ax->stringCheck(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR));
    bool yFlip = (ax->stringCheck(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR));
    RGBAImageView view;
    view.width = imgWidth;
    view.height = imgHeight;
    view.bytesPerLine = imgWidth * 4;
    view.pixels = imgRgba.data();
    view.resourceKey = imageResourceKey;
    view.contentRevision = imageRevision;
    view.opaque = imageOpaque;
    std::vector<double> xSpan(
        imageCoordinateSpan(imageCoordinatesOrDefault(xp->data(), imgWidth), imgWidth));
    std::vector<double> ySpan(
        imageCoordinateSpan(imageCoordinatesOrDefault(yp->data(), imgHeight), imgHeight));
    if (xSpan.size() < 2 || ySpan.size() < 2) {
        return;
    }
    gc.drawImage(xSpan, ySpan, xLim->data(), yLim->data(), xFlip, yFlip, view);
}
//=============================================================================
std::vector<double>
GOImage::getAlphaMap(indexType rows, indexType cols)
{
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR);
    if (!hp) {
        return std::vector<double>(rows * cols, 1.0);
    }
    std::vector<double> alphain(hp->data());
    std::vector<double> alphaout;
    alphaout.reserve(rows * cols);
    GOFigure* fig = getParentFigure();
    GOAxis* ap = getParentAxis();
    if (!fig || !ap) {
        return std::vector<double>(rows * cols, 1.0);
    }
    std::vector<double> amap(
        ((GraphicsObject*)fig)->findVectorDoubleProperty(GO_ALPHA_MAP_PROPERTY_NAME_STR));
    indexType amaplen((indexType)amap.size());
    std::vector<double> alim(
        ((GraphicsObject*)ap)->findVectorDoubleProperty(GO_A_LIM_PROPERTY_NAME_STR));
    if (alim.size() < 2) {
        alim = { 0.0, 1.0 };
    }
    double alim_min(std::min(alim[0], alim[1]));
    double alim_max(std::max(alim[0], alim[1]));
    if (amaplen == 0) {
        return std::vector<double>(rows * cols, 1.0);
    }
    int increment;
    if (alphain.size() == 0) {
        for (indexType i = 0; i < rows * cols; i++) {
            alphaout.push_back(1);
        }
        return alphaout;
    } else if (alphain.size() != rows * cols) {
        increment = 0;
    } else {
        increment = 1;
    }
    if (stringCheck(GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        alphaout.resize(rows * cols);
        OMP_PARALLEL_FOR_LOOP(rows * cols)
        for (indexType i = 0; i < rows * cols; i++) {
            alphaout[i] = std::min(1.0, std::max(0.0, alphain[i * increment]));
        }
    } else if (stringCheck(GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DIRECT_STR)) {
        alphaout.resize(rows * cols);
        OMP_PARALLEL_FOR_LOOP(rows * cols)
        for (indexType i = 0; i < rows * cols; i++) {
            indexType ndx = (indexType)alphain[i * increment] - 1;
            ndx = std::min<indexType>(amaplen - 1, std::max<indexType>(0, ndx));
            alphaout[i] = amap[ndx];
        }
    } else {
        alphaout.resize(rows * cols);
        if (!(alim_max > alim_min)) {
            OMP_PARALLEL_FOR_LOOP(rows * cols)
            for (indexType i = 0; i < rows * cols; i++) {
                alphaout[i] = amap[0];
            }
            return alphaout;
        }
        OMP_PARALLEL_FOR_LOOP(rows * cols)
        for (indexType i = 0; i < rows * cols; i++) {
            indexType ndx = (indexType)alphain[i * increment] - 1;
            ndx = (indexType)((alphain[i * increment] - alim_min) / (alim_max - alim_min)
                * (amaplen - 1));
            ndx = std::min<indexType>(amaplen - 1, std::max<indexType>(0, ndx));
            alphaout[i] = amap[ndx];
        }
    }
    return alphaout;
}
//=============================================================================
bool
GOImage::usesUnitAlpha()
{
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR);
    if (!hp) {
        return true;
    }
    const std::vector<double>& alpha = hp->data();
    return alpha.size() == 1 && std::fabs(alpha[0] - 1.0) < std::numeric_limits<double>::epsilon()
        && stringCheck(GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
}
//=============================================================================
void
GOImage::prepareMappedRGBA(const double* dp, indexType rows, indexType cols,
    const std::vector<double>* alpha, bool floatData)
{
    imgWidth = static_cast<int>(cols);
    imgHeight = static_cast<int>(rows);
    const indexType elementCount = rows * cols;
    imgRgba.resize(static_cast<size_t>(elementCount) * 4);
    unsigned char* rgba = imgRgba.data();
    GOAxis* ap(getParentAxis());
    std::vector<double> cmap;
    std::vector<double> clim;
    if (ap) {
        cmap = ((GraphicsObject*)ap)->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
        clim = ((GraphicsObject*)ap)->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR);
    }
    const size_t cmaplen = cmap.size() / 3;
    if (!dp || cmaplen < 1 || clim.size() < 2) {
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (indexType idx = 0; idx < elementCount; idx++) {
            indexType row = idx % rows;
            indexType col = idx / rows;
            unsigned char* px = rgba + (static_cast<size_t>(row) * cols + col) * 4;
            px[0] = 255;
            px[1] = 255;
            px[2] = 255;
            px[3] = alphaAt(alpha, idx);
        }
        return;
    }
    const double clim_min = std::min(clim[0], clim[1]);
    const double clim_max = std::max(clim[0], clim[1]);
    if (std::fabs(clim_min - clim_max) < std::numeric_limits<double>::epsilon()) {
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (indexType idx = 0; idx < elementCount; idx++) {
            indexType row = idx % rows;
            indexType col = idx / rows;
            unsigned char* px = rgba + (static_cast<size_t>(row) * cols + col) * 4;
            px[0] = byteFromUnit(cmap[0]);
            px[1] = byteFromUnit(cmap[1]);
            px[2] = byteFromUnit(cmap[2]);
            px[3] = alphaAt(alpha, idx);
        }
        return;
    }
    if (stringCheck(GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DIRECT_STR)) {
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (indexType idx = 0; idx < elementCount; idx++) {
            int ndx = floatData ? static_cast<int>(dp[idx]) - 1 : static_cast<int>(dp[idx]);
            // Clamp in signed arithmetic: a direct index of 0 (CData 0 gives -1 after the 1-based
            // to 0-based shift) must land on the first colormap row, not wrap through the unsigned
            // indexType to the last row.
            ndx = std::min(static_cast<int>(cmaplen - 1), std::max(0, ndx));
            indexType row = idx % rows;
            indexType col = idx / rows;
            unsigned char* px = rgba + (static_cast<size_t>(row) * cols + col) * 4;
            px[0] = byteFromUnit(cmap[3 * ndx]);
            px[1] = byteFromUnit(cmap[3 * ndx + 1]);
            px[2] = byteFromUnit(cmap[3 * ndx + 2]);
            px[3] = alphaAt(alpha, idx);
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (indexType idx = 0; idx < elementCount; idx++) {
            double scaled = (dp[idx] - clim_min) / (clim_max - clim_min);
            int ndx = static_cast<int>(std::floor(scaled * cmaplen));
            ndx = (int)std::min<indexType>(cmaplen - 1, std::max(0, ndx));
            indexType row = idx % rows;
            indexType col = idx / rows;
            unsigned char* px = rgba + (static_cast<size_t>(row) * cols + col) * 4;
            px[0] = byteFromUnit(cmap[3 * ndx]);
            px[1] = byteFromUnit(cmap[3 * ndx + 1]);
            px[2] = byteFromUnit(cmap[3 * ndx + 2]);
            px[3] = alphaAt(alpha, idx);
        }
    }
}
//=============================================================================
void
GOImage::prepareTrueColorRGBA(const double* dp, indexType rows, indexType cols,
    const std::vector<double>* alpha, bool isIntegerData)
{
    imgWidth = static_cast<int>(cols);
    imgHeight = static_cast<int>(rows);
    const indexType elementCount = rows * cols;
    imgRgba.resize(static_cast<size_t>(elementCount) * 4);
    unsigned char* rgba = imgRgba.data();
    if (alpha == nullptr) {
        OMP_PARALLEL_FOR_LOOP(rows)
        for (indexType row = 0; row < rows; row++) {
            unsigned char* px = rgba + static_cast<size_t>(row) * cols * 4;
            const double* r = dp + row;
            const double* g = r + elementCount;
            const double* b = g + elementCount;
            for (indexType col = 0; col < cols; col++) {
                px[0] = isIntegerData ? byteFromScaled(*r) : byteFromUnit(*r);
                px[1] = isIntegerData ? byteFromScaled(*g) : byteFromUnit(*g);
                px[2] = isIntegerData ? byteFromScaled(*b) : byteFromUnit(*b);
                px[3] = 255;
                px += 4;
                r += rows;
                g += rows;
                b += rows;
            }
        }
        return;
    }
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (indexType idx = 0; idx < elementCount; idx++) {
        indexType i = idx % rows;
        indexType j = idx / rows;

        indexType baseIndex = idx;
        unsigned char r
            = isIntegerData ? byteFromScaled(dp[baseIndex]) : byteFromUnit(dp[baseIndex]);
        unsigned char g = isIntegerData ? byteFromScaled(dp[baseIndex + elementCount])
                                        : byteFromUnit(dp[baseIndex + elementCount]);
        unsigned char b = isIntegerData ? byteFromScaled(dp[baseIndex + 2 * elementCount])
                                        : byteFromUnit(dp[baseIndex + 2 * elementCount]);

        unsigned char* px = rgba + ((size_t)i * cols + j) * 4;
        px[0] = r;
        px[1] = g;
        px[2] = b;
        px[3] = alphaAt(alpha, baseIndex);
    }
}
//=============================================================================
void
GOImage::prepareTrueColorRGBA8(
    const unsigned char* dp, indexType rows, indexType cols, const std::vector<double>* alpha)
{
    imgWidth = static_cast<int>(cols);
    imgHeight = static_cast<int>(rows);
    const indexType elementCount = rows * cols;
    imgRgba.resize(static_cast<size_t>(elementCount) * 4);
    unsigned char* rgba = imgRgba.data();
    if (alpha == nullptr) {
        OMP_PARALLEL_FOR_LOOP(rows)
        for (indexType row = 0; row < rows; row++) {
            unsigned char* px = rgba + static_cast<size_t>(row) * cols * 4;
            const unsigned char* r = dp + row;
            const unsigned char* g = r + elementCount;
            const unsigned char* b = g + elementCount;
            for (indexType col = 0; col < cols; col++) {
                px[0] = *r;
                px[1] = *g;
                px[2] = *b;
                px[3] = 255;
                px += 4;
                r += rows;
                g += rows;
                b += rows;
            }
        }
        return;
    }
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (indexType idx = 0; idx < elementCount; idx++) {
        const indexType row = idx % rows;
        const indexType col = idx / rows;
        unsigned char* px = rgba + (static_cast<size_t>(row) * cols + col) * 4;
        px[0] = dp[idx];
        px[1] = dp[idx + elementCount];
        px[2] = dp[idx + 2 * elementCount];
        px[3] = alphaAt(alpha, idx);
    }
}
//=============================================================================
// The image is drawn between the first and the last coordinate it was given. A
// single coordinate places the first column (or row) and leaves every following
// one a unit further along, which is what a lone XData value means.
std::vector<double>
imageCoordinateSpan(const std::vector<double>& coordinates, indexType count)
{
    if (coordinates.empty()) {
        return {};
    }
    double first = coordinates.front();
    double last = (coordinates.size() > 1)
        ? coordinates.back()
        : first + static_cast<double>((count > 1) ? (count - 1) : 0);
    return { first, last };
}
//=============================================================================
// An unset span means "the extent of CData". updateState() stores that default as
// soon as it runs, but the limits and the drawing must not depend on having been
// through it: an image asked for its limits first would otherwise report none and
// let the axes scale itself on nothing.
std::vector<double>
imageCoordinatesOrDefault(const std::vector<double>& coordinates, indexType count)
{
    if (!coordinates.empty()) {
        return coordinates;
    }
    if (count > 1) {
        return { 1., static_cast<double>(count) };
    }
    if (count == 1) {
        return { 1. };
    }
    return {};
}
//=============================================================================
void
setImageCoordinates(GOGenericProperty* property, const std::vector<double>& coordinates)
{
    GOVectorProperty* hp = (GOVectorProperty*)property;
    if (hp == nullptr) {
        return;
    }
    hp->data(coordinates);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
