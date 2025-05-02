//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPatch.hpp"
#include "GOAxis.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "RenderInterface.hpp"
#include "GOVectorProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "GOMappingModeProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOBackFaceLightingProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GODataMappingModeProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOEgdeAlphaProperty.hpp"
#include "GOColorInterpProperty.hpp"
#include "GOLightingModeProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GORowColumnProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "RGBAColorData.hpp"
#include "RenderHelpers.hpp"
#include "GOColorProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOPatch::GOPatch()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOPatch::~GOPatch() = default;
//=============================================================================
std::wstring
GOPatch::getType()
{
    return GO_PROPERTY_VALUE_PATCH_STR;
}
//=============================================================================
void
GOPatch::updateState()
{
    if (m_faces.empty() || hasChanged(GO_FACES_PROPERTY_NAME_STR)
        || hasChanged(GO_VERTICES_PROPERTY_NAME_STR)
        || hasChanged(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_FACE_COLOR_PROPERTY_NAME_STR)
        || hasChanged(GO_EDGE_COLOR_PROPERTY_NAME_STR)) {
        limitsDirty = true;
        m_faces.clear();
        buildPolygons(m_faces);
        clearChanged(GO_FACES_PROPERTY_NAME_STR);
        clearChanged(GO_VERTICES_PROPERTY_NAME_STR);
        clearChanged(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR);
        clearChanged(GO_FACE_COLOR_PROPERTY_NAME_STR);
        clearChanged(GO_EDGE_COLOR_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOPatch::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    double lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    std::wstring lineStyle = findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR);

    gc.drawPatch(m_faces, lineWidth, lineStyle);
}
//=============================================================================
std::vector<double>
GOPatch::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    limits.resize(0);
    limits.reserve(10);

    ArrayOf vertices(findArrayOfProperty(GO_VERTICES_PROPERTY_NAME_STR));
    vertices.promoteType(NLS_DOUBLE);
    indexType nRows = vertices.getRows();

    const double* pVerticesDataX = (const double*)vertices.getDataPointer();
    const double* pVerticesDataY = nullptr;
    if (pVerticesDataX) {
        pVerticesDataY = pVerticesDataX + nRows;
    }
    const double* pVertDataZ = nullptr;
    if (pVerticesDataY) {
        pVertDataZ = pVerticesDataY + nRows;
    }
    double Xmax = 1;
    double Xmin = 1;
    double Ymax = 1;
    double Ymin = 1;
    double Zmax = 1;
    double Zmin = 1;

    if (nRows > 0) {
        Xmax = *pVerticesDataX;
        Xmin = Xmax;
        Ymax = *pVerticesDataY;
        Ymin = Ymax;
        if (pVertDataZ) {
            Zmax = *pVertDataZ;
        }
        Zmin = Zmax;

        for (indexType i = 0; i < nRows; i++) {
            Xmax = std::max(Xmax, *pVerticesDataX);
            Xmin = std::min(Xmin, *pVerticesDataX);
            Ymax = std::max(Ymax, *pVerticesDataY);
            Ymin = std::min(Ymin, *pVerticesDataY);
            Zmax = std::max(Zmax, *pVertDataZ);
            Zmin = std::min(Zmin, *pVertDataZ);
            ++pVerticesDataX;
            ++pVerticesDataY;
            ++pVertDataZ;
        }
    }

    ArrayOf faceVertexCData(findArrayOfProperty(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR));
    faceVertexCData.promoteType(NLS_DOUBLE);

    double CLimMin = ArrayOfMin(faceVertexCData);
    double CLimMax = ArrayOfMax(faceVertexCData);

    limits.push_back(Xmin);
    limits.push_back(Xmax);
    limits.push_back(Ymin);
    limits.push_back(Ymax);
    limits.push_back(Zmin);
    limits.push_back(Zmax);
    limits.push_back(CLimMin);
    limits.push_back(CLimMax);
    limits.push_back(1.);
    limits.push_back(1.);
    limitsDirty = false;
    return limits;
}
//=============================================================================
void
GOPatch::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOMappingModeProperty, GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOBackFaceLightingProperty, GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GODataMappingModeProperty, GO_C_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOEdgeAlphaProperty, GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_EDGE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_FACES_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VERTEX_NORMALS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VERTICES_PROPERTY_NAME_STR);
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
GOPatch::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(
        GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setScalarDoubleDefault(GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR, 0.3);
    setRestrictedStringDefault(
        GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSELIT_STR);
    setRestrictedStringDefault(GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setScalarDoubleDefault(GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR, 0.6);
    setScalarDoubleDefault(GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR, 1);
    setScalarDoubleDefault(GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR, 10);
    setScalarDoubleDefault(GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR, 0.9);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PATCH_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringScalarDefault(
        GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_EDGE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0, 0, 0);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PATCH_STR);
    setStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
inline const double*
getVerticesData(const double* pVertData, indexType i, indexType j, indexType nVertices,
    indexType nbColumnsVerticesData)
{
    return ((i < nVertices && j < nbColumnsVerticesData) ? (pVertData + i + nVertices * j)
                                                         : (pVertData));
}
//=============================================================================
inline const double*
getVerticesColor(const double* pVertColor, indexType i, indexType j,
    indexType nbRowsFaceVertexCdata, indexType nbColumnsFaceVertexCdata)
{
    return ((i < nbRowsFaceVertexCdata && j < nbColumnsFaceVertexCdata)
            ? (pVertColor + i + (int)nbRowsFaceVertexCdata * j)
            : (pVertColor));
}
//=============================================================================
void
GOPatch::buildPolygons(FaceList& faces)
{
    ArrayOf facesData(findArrayOfProperty(GO_FACES_PROPERTY_NAME_STR));
    facesData.promoteType(NLS_DOUBLE);

    ArrayOf verticesdata(findArrayOfProperty(GO_VERTICES_PROPERTY_NAME_STR));
    verticesdata.promoteType(NLS_DOUBLE);
    indexType nbColumnsVerticesData = verticesdata.getColumns();
    indexType nVertices = verticesdata.getRows();

    ArrayOf faceVertexCdata(findArrayOfProperty(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR));
    faceVertexCdata.promoteType(NLS_DOUBLE);

    ColorMode::ColorMode FaceColorMode = ColorMode::ColorMode::None;
    ColorMode::ColorMode EdgeColorMode = ColorMode::ColorMode::None;

    if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)) {
        FaceColorMode = ColorMode::Flat;
    } else if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        FaceColorMode = ColorMode::None;
    } else if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INTERP_STR)) {
        FaceColorMode = ColorMode::Interp;
    } else {
        FaceColorMode = ColorMode::ColorSpec;
    }

    if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)) {
        EdgeColorMode = ColorMode::Flat;
    } else if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        EdgeColorMode = ColorMode::None;
    } else if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INTERP_STR)) {
        EdgeColorMode = ColorMode::Interp;
    } else {
        EdgeColorMode = ColorMode::ColorSpec;
    }

    if (verticesdata.isEmpty() || facesData.isEmpty()) {
        return;
    }

    if (nbColumnsVerticesData != 3) {
        Error(_("Nx3 dimensional matrix expected for 'Vertices'."));
    }

    const double* pVertOrder = static_cast<const double*>(facesData.getDataPointer());
    const double* pVertData = static_cast<const double*>(verticesdata.getDataPointer());
    const double* pVertColor = static_cast<const double*>(faceVertexCdata.getDataPointer());

    GOFaceAlphaProperty* fa
        = static_cast<GOFaceAlphaProperty*>(findProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
    double alphaLevel = fa->scalar();

    indexType nbColumnsFaceVertexCdata = faceVertexCdata.getColumns();
    indexType nbRowsFaceVertexCdata = faceVertexCdata.getRows();

    indexType nFaces = facesData.getRows();
    indexType maxVertsPerFace = facesData.getColumns();
    faces.reserve(nFaces);
    for (indexType j = 0; j < nFaces; j++) {
        Face face;
        face.FaceColorMode = FaceColorMode;
        face.EdgeColorMode = EdgeColorMode;

        if (face.FaceColorMode == ColorMode::ColorSpec) {
            GORestrictedStringColorProperty* fc = static_cast<GORestrictedStringColorProperty*>(
                findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR));
            if (!fc) {
                Error(_W("Invalid Face Colorspec."));
            } else {
                std::vector<double> colorspec = fc->colorSpec();
                face.FaceColor
                    = RGBAColorData(colorspec[0], colorspec[1], colorspec[2], alphaLevel);
            }
        }

        if (face.EdgeColorMode == ColorMode::ColorSpec) {
            GORestrictedStringColorProperty* ec = static_cast<GORestrictedStringColorProperty*>(
                findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR));
            if (!ec) {
                Error(_W("Invalid EdgeColor parameter."));
            }
            std::vector<double> colorspec = ec->colorSpec();
            face.EdgeColor = RGBAColorData(colorspec[0], colorspec[1], colorspec[2], 1);
        }

        if (face.EdgeColorMode == ColorMode::Flat && (nbColumnsFaceVertexCdata != 3)
            && ((nbRowsFaceVertexCdata != 1) || ((nbRowsFaceVertexCdata != nVertices)))) {
            Error(_W("Invalid FaceVertexCData parameter with EgdeColor to 'flat'."));
        }

        if (face.EdgeColorMode == ColorMode::Interp && (nbColumnsFaceVertexCdata != 3)
            && (nbRowsFaceVertexCdata != nVertices)) {
            Error(_W("Invalid FaceVertexCData parameter with EgdeColor to 'interp'."));
        }

        for (int k = 0; k < maxVertsPerFace; k++) {
            if (!std::isnan(*(pVertOrder + j + k * nFaces))) {
                point vert;
                int vertIndex = (int)(*(pVertOrder + j + k * nFaces)) - 1;

                if (vertIndex >= nVertices || vertIndex < 0) {
                    Error(_W("Vertex Index out of bounds."));
                }

                vert.x
                    = *(getVerticesData(pVertData, vertIndex, 0, nVertices, nbColumnsVerticesData));

                vert.y
                    = *(getVerticesData(pVertData, vertIndex, 1, nVertices, nbColumnsVerticesData));

                vert.z
                    = *(getVerticesData(pVertData, vertIndex, 2, nVertices, nbColumnsVerticesData));

                face.vertices.push_back(vert);

                double R = 0;
                double G = 0;
                double B = 0;
                double A = alphaLevel;
                if (face.FaceColorMode == ColorMode::Flat) {
                    if (nbRowsFaceVertexCdata == 3) {
                        R = pVertColor[0];
                        G = pVertColor[1];
                        B = pVertColor[2];
                    } else {
                        indexType firstVertIndex = (nbRowsFaceVertexCdata != 1)
                            ? (indexType)(*(pVertOrder + j + k * nFaces) - 1)
                            : 0;
                        firstVertIndex = (nbRowsFaceVertexCdata == nFaces) ? j : firstVertIndex;

                        R = *(getVerticesColor(pVertColor, firstVertIndex, 0, nbRowsFaceVertexCdata,
                            nbColumnsFaceVertexCdata));
                        G = *(getVerticesColor(pVertColor, firstVertIndex, 1, nbRowsFaceVertexCdata,
                            nbColumnsFaceVertexCdata));
                        B = *(getVerticesColor(pVertColor, firstVertIndex, 2, nbRowsFaceVertexCdata,
                            nbColumnsFaceVertexCdata));
                    }
                    RGBAColorData vertColor(R, G, B, A);
                    face.vertexcolors.push_back(vertColor);
                } else if (face.FaceColorMode == ColorMode::Interp) {
                    R = *(getVerticesColor(
                        pVertColor, vertIndex, 0, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                    G = *(getVerticesColor(
                        pVertColor, vertIndex, 1, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                    B = *(getVerticesColor(
                        pVertColor, vertIndex, 2, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));

                    RGBAColorData vertColor(R, G, B, A);
                    face.vertexcolors.push_back(vertColor);
                }
                if (face.EdgeColorMode == ColorMode::Flat) {
                    indexType firstVertIndex = (nbRowsFaceVertexCdata != 1)
                        ? (indexType)(*(pVertOrder + j + k * nFaces) - 1)
                        : 0;
                    firstVertIndex = (nbRowsFaceVertexCdata == nFaces) ? j : firstVertIndex;
                    R = *(getVerticesColor(pVertColor, firstVertIndex, 0, nbRowsFaceVertexCdata,
                        nbColumnsFaceVertexCdata));
                    G = *(getVerticesColor(pVertColor, firstVertIndex, 1, nbRowsFaceVertexCdata,
                        nbColumnsFaceVertexCdata));
                    B = *(getVerticesColor(pVertColor, firstVertIndex, 2, nbRowsFaceVertexCdata,
                        nbColumnsFaceVertexCdata));
                    RGBAColorData vertColor(R, G, B, A);
                    face.edgecolors.push_back(vertColor);
                } else if (face.EdgeColorMode == ColorMode::Interp) {
                    R = *(getVerticesColor(
                        pVertColor, vertIndex, 0, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                    G = *(getVerticesColor(
                        pVertColor, vertIndex, 1, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                    B = *(getVerticesColor(
                        pVertColor, vertIndex, 2, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                    RGBAColorData vertColor(R, G, B, A);
                    face.edgecolors.push_back(vertColor);
                }
            }
        }
        faces.push_back(face);
    }
}
//=============================================================================
}
//=============================================================================
