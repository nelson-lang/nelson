//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOSurface.hpp"
#include "GOAxis.hpp"
#include "GOBackFaceLightingProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOColorInterpProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOLightingModeProperty.hpp"
#include "GORowColumnProperty.hpp"
#include "GODataMappingModeProperty.hpp"
#include "GOMappingModeProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GORestrictedStringColorProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOEgdeAlphaProperty.hpp"
#include "GORestrictedStringScalarProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOSurface::getType()
{
    return GO_PROPERTY_VALUE_SURFACE_STR;
}
//=============================================================================
GOSurface::GOSurface()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOSurface::~GOSurface() { }
//=============================================================================
void
GOSurface::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_ALPHA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOMappingModeProperty, GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOBackFaceLightingProperty, GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GODataMappingModeProperty, GO_C_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_C_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOEdgeAlphaProperty, GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_EDGE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GORowColumnProperty, GO_MESH_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VERTEX_NORMALS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOSurface::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR);
    std::vector<double> gp;
    gp.push_back(1.0);
    hp->data(gp);
    setRestrictedStringDefault(GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setScalarDoubleDefault(GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR, 0.3);
    setRestrictedStringDefault(
        GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_UNLIT_STR);
    setRestrictedStringDefault(GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setRestrictedStringDefault(GO_C_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR, 0.6);
    setScalarDoubleDefault(GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR, 0.4);
    setScalarDoubleDefault(GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR, 0.1);
    setScalarDoubleDefault(GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR, 0.5);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_IMAGE_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringScalarDefault(
        GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_EDGE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringDefault(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR);
    setRestrictedStringDefault(GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0, 0, 0);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setRestrictedStringDefault(GO_MESH_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
std::vector<std::vector<coloredPoint>>
GOSurface::buildQuadsNoTexMap(
    GORestrictedStringColorProperty* cp, GORestrictedStringScalarProperty* ap)
{
    std::vector<std::vector<coloredPoint>> retval;
    ArrayOf xdata(getCoordinateMatrix(GO_X_DATA_PROPERTY_NAME_STR, true));
    ArrayOf ydata(getCoordinateMatrix(GO_Y_DATA_PROPERTY_NAME_STR, false));
    ydata.promoteType(NLS_DOUBLE);
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    zdata.promoteType(NLS_DOUBLE);
    if ((xdata.getElementCount() != zdata.getElementCount())
        || (xdata.getElementCount() != ydata.getElementCount())) {
        return retval;
    }
    if (zdata.isEmpty()) {
        return retval;
    }
    double* xdp = (double*)xdata.getDataPointer();
    double* ydp = (double*)ydata.getDataPointer();
    double* zdp = (double*)zdata.getDataPointer();
    indexType rows = zdata.getRows();
    indexType cols = zdata.getColumns();
    if (cp->isEqual(GO_PROPERTY_VALUE_INTERP_STR)
        && ((img.height() < rows) || (img.width() < cols)))
        return retval;
    if (ap->isEqual(GO_PROPERTY_VALUE_INTERP_STR)
        && ((img.height() < rows) || (img.width() < cols)))
        return retval;
    if (cp->isEqual(GO_PROPERTY_VALUE_FLAT_STR)
        && ((img.height() < rows - 1) || (img.width() < cols - 1)))
        return retval;
    if (ap->isEqual(GO_PROPERTY_VALUE_FLAT_STR)
        && ((img.height() < rows - 1) || (img.width() < cols - 1)))
        return retval;
    if (cp->isEqual(GO_PROPERTY_VALUE_NONE_STR))
        return retval;
    QRgb* dummyline = nullptr;
    if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR) || ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
        dummyline = new QRgb[cols];
        double r = 0;
        double g = 0;
        double b = 0;
        double alphaval = 1.0;
        if (ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR))
            alphaval = ap->scalar();
        if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)) {
            std::vector<double> p(cp->colorSpec());
            if (p[0] == -1)
                return retval;
            r = p[0];
            g = p[1];
            b = p[2];
        }
        for (indexType i = 0; i < cols; i++)
            dummyline[i]
                = qRgba((int)(255 * r), (int)(255 * g), (int)(255 * b), (int)(255 * alphaval));
    }
    for (int i = 0; i < rows - 1; i++) {
        QRgb *cbits1 = nullptr, *cbits2 = nullptr, *abits1 = nullptr, *abits2 = nullptr;
        int col_lim, alp_lim;
        if (cp->isEqual(GO_PROPERTY_VALUE_INTERP_STR)) {
            cbits1 = (QRgb*)img.scanLine(i);
            cbits2 = (QRgb*)img.scanLine(i + 1);
            col_lim = (int)(cols - 1);
        } else if (cp->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            cbits1 = (QRgb*)img.scanLine(i);
            cbits2 = (QRgb*)img.scanLine(i);
            col_lim = (int)(cols - 2);
        } else if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)) {
            cbits1 = (QRgb*)dummyline;
            cbits2 = (QRgb*)dummyline;
            col_lim = (int)(cols - 1);
        }
        if (ap->isEqual(GO_PROPERTY_VALUE_INTERP_STR)) {
            abits1 = (QRgb*)img.scanLine(i);
            abits2 = (QRgb*)img.scanLine(i + 1);
            alp_lim = (int)(cols - 1);
        } else if (ap->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            abits1 = (QRgb*)img.scanLine(i);
            abits2 = (QRgb*)img.scanLine(i);
            alp_lim = (int)(cols - 2);
        } else if (ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
            if (dummyline != nullptr) {
                abits1 = (QRgb*)dummyline;
                abits2 = (QRgb*)dummyline;
            }
            alp_lim = (int)(cols - 1);
        }
        std::vector<coloredPoint> linequads;
        for (int j = 0; j < cols; j++) {
            int ccol = std::min(j, col_lim);
            int acol = std::min(j, alp_lim);
            if (cbits1 && cbits2 && abits2 && abits1) {
                linequads.push_back(coloredPoint(xdp[i + j * rows], ydp[i + j * rows],
                    zdp[i + j * rows], qRed(cbits1[ccol]) / 255.0, qGreen(cbits1[ccol]) / 255.0,
                    qBlue(cbits1[ccol]) / 255.0, qAlpha(abits1[acol]) / 255.0));
                linequads.push_back(coloredPoint(xdp[i + 1 + j * rows], ydp[i + 1 + j * rows],
                    zdp[i + 1 + j * rows], qRed(cbits2[ccol]) / 255.0, qGreen(cbits2[ccol]) / 255.0,
                    qBlue(cbits2[ccol]) / 255.0, qAlpha(abits2[acol]) / 255.0));
            }
        }
        retval.push_back(linequads);
    }
    if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR) || ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR))
        delete[] dummyline;
    return retval;
}
//=============================================================================
ArrayOf
GOSurface::getCoordinateMatrix(const std::wstring& propertyName, bool isXCoord)
{
    ArrayOf ZData(findArrayOfProperty(L"ZData"));
    indexType zRows = ZData.getRows();
    indexType zCols = ZData.getColumns();
    if (stringCheck(propertyName + L"Mode", L"manual")) {
        ArrayOf CData(findArrayOfProperty(propertyName));
        if (CData.isVector()
            && ((isXCoord && (CData.getElementCount() == zCols))
                || (!isXCoord && (CData.getElementCount() == zRows)))) {
            CData.promoteType(NLS_DOUBLE);
            const double* qp = (const double*)CData.getDataPointer();
            Dimensions dimsZ(zRows, zCols);
            double* dp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsZ.getElementCount());
            ArrayOf mat = ArrayOf(NLS_DOUBLE, dimsZ, dp);
            for (indexType i = 0; i < zCols; i++) {
                for (indexType j = 0; j < zRows; j++) {
                    if (isXCoord) {
                        *dp = qp[i];
                    } else {
                        *dp = qp[j];
                    }
                    dp++;
                }
            }
            return mat;
        } else if (CData.is2D() && (CData.getRows() == zRows) && (CData.getColumns() == zCols)) {
            return CData;
        }
    }
    Dimensions dimsZ(zRows, zCols);
    double* dp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsZ.getElementCount());
    ArrayOf mat = ArrayOf(NLS_DOUBLE, dimsZ, dp);
    for (indexType i = 0; i < zCols; i++) {
        for (indexType j = 0; j < zRows; j++) {
            if (isXCoord) {
                *dp = i + 1;
            } else {
                *dp = j + 1;
            }
            dp++;
        }
    }
    return mat;
}
//=============================================================================
void
GOSurface::updateState()
{
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_X_DATA_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_Y_DATA_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_C_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        autoXMode();
        clearChanged(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_Y_DATA_MODE_PROPERTY_NAME_STR)) {
        autoYMode();
        clearChanged(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_C_DATA_MODE_PROPERTY_NAME_STR)) {
        autoCMode();
        clearChanged(GO_C_DATA_MODE_PROPERTY_NAME_STR);
    }
    updateCAlphadata();
}
//=============================================================================
void
GOSurface::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    ArrayOf xdata(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    xdata.promoteType(NLS_DOUBLE);
    ArrayOf ydata(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    ydata.promoteType(NLS_DOUBLE);
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (!zdata.isNumeric()) {
        return;
    }
    zdata.promoteType(NLS_DOUBLE);
    if (zdata.isEmpty()) {
        return;
    }
    if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXTUREMAP_STR)) {
        return;
    }
    if (stringCheck(GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXTUREMAP_STR)) {
        return;
    }
    std::vector<std::vector<coloredPoint>> surfquads(buildQuadsNoTexMap(
        (GORestrictedStringColorProperty*)findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR),
        (GORestrictedStringScalarProperty*)findProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR)));
    std::vector<std::vector<coloredPoint>> edgequads(buildQuadsNoTexMap(
        (GORestrictedStringColorProperty*)findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR),
        (GORestrictedStringScalarProperty*)findProperty(GO_EDGE_ALPHA_PROPERTY_NAME_STR)));

    GORestrictedStringScalarProperty* meshStyleProperty
        = static_cast<GORestrictedStringScalarProperty*>(
            findProperty(GO_MESH_STYLE_PROPERTY_NAME_STR));
    meshStyle meshstyle = meshStyle::Both;
    if (meshStyleProperty->isEqual(GO_PROPERTY_VALUE_BOTH_STR)) {
        meshstyle = meshStyle::Both;
    } else if (meshStyleProperty->isEqual(GO_PROPERTY_VALUE_ROW_STR)) {
        meshstyle = meshStyle::Row;
    } else {
        meshstyle = meshStyle::Column;
    }
    std::wstring lineStyle
        = ((GORestrictedStringColorProperty*)findProperty(GO_LINE_STYLE_PROPERTY_NAME_STR))->data();

    if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        lineStyle = GO_PROPERTY_VALUE_NONE_STR;
    }

    double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    gc.quadStrips(surfquads,
        stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR), edgequads,
        stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR), meshstyle,
        lineWidth, lineStyle);
}
//=============================================================================
std::vector<double>
GOSurface::getLimits()
{
    std::vector<double> limits;
    limits.reserve(10);
    ArrayOf xdata(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    ArrayOf ydata(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    limits.push_back(ArrayOfMin(xdata));
    limits.push_back(ArrayOfMax(xdata));
    limits.push_back(ArrayOfMin(ydata));
    limits.push_back(ArrayOfMax(ydata));
    limits.push_back(ArrayOfMin(zdata));
    limits.push_back(ArrayOfMax(zdata));
    limits.push_back(ArrayOfMin(cdata));
    limits.push_back(ArrayOfMax(cdata));
    std::vector<double> alphadata(findVectorDoubleProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR));
    limits.push_back(findVectorMin(alphadata));
    limits.push_back(findVectorMax(alphadata));
    return limits;
}
//=============================================================================
void
GOSurface::autoCMode()
{
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_C_DATA_PROPERTY_NAME_STR);
    hp->data(zdata);
}
//=============================================================================
void
GOSurface::autoYMode()
{
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    ArrayOf ydata(ArrayOf::doubleMatrix2dConstructor(zdata.getRows(), zdata.getColumns()));
    double* dp = (double*)ydata.getReadWriteDataPointer();
    indexType cols(zdata.getColumns());
    indexType rows(zdata.getRows());
    OMP_PARALLEL_FOR_LOOP(rows * cols)
    for (ompIndexType idx = 0; idx < (ompIndexType)(rows * cols); idx++) {
        ompIndexType j = idx / rows;
        indexType i = idx % rows;
        dp[i + j * rows] = i + 1;
    }
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    hp->data(ydata);
}
//=============================================================================
void
GOSurface::autoXMode()
{
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    ArrayOf xdata(ArrayOf::doubleMatrix2dConstructor(zdata.getRows(), zdata.getColumns()));
    double* dp = (double*)xdata.getReadWriteDataPointer();
    indexType cols(zdata.getColumns());
    indexType rows(zdata.getRows());
    OMP_PARALLEL_FOR_LOOP(rows * cols)
    for (ompIndexType idx = 0; idx < (ompIndexType)(rows * cols); idx++) {
        ompIndexType j = idx / rows;
        indexType i = idx % rows;
        dp[i + j * rows] = j + 1;
    }
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_X_DATA_PROPERTY_NAME_STR);
    hp->data(xdata);
}
//=============================================================================
}
//=============================================================================
