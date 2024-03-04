//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
#include "RenderInterface.hpp"
#include "ContourPoint.hpp"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * @brief Represents a contour graphics object.
 */
class NLSGRAPHICS_IMPEXP GOContour : public GraphicsObject
{
public:
    /**
     * @brief Constructor for GOContour.
     */
    GOContour();
    //=============================================================================
    /**
     * @brief Destructor for GOContour.
     */
    virtual ~GOContour();
    //=============================================================================
    /**
     * @brief Gets the type of the graphics object.
     *
     * @return The type of the graphics object.
     */
    std::wstring
    getType() override;
    //=============================================================================
    /**
     * @brief Updates the state of the graphics object.
     */
    virtual void
    updateState();
    //=============================================================================
    /**
     * @brief Paints the graphics object.
     *
     * @param gc The render interface for painting.
     */
    virtual void
    paintMe(RenderInterface& gc);
    //=============================================================================
private:
    contourLineCollection contourLines; /**< The collection of contour lines */
    std::vector<double> zValues; /**< The z-values of the contour */
    //=============================================================================
    /**
     * @brief Constructs properties for the contour.
     */
    virtual void
    constructProperties();
    //=============================================================================
    /**
     * @brief Sets up default values for the contour.
     */
    virtual void
    setupDefaults();
    //=============================================================================
    /**
     * @brief Selects color for rendering based on z-value.
     *
     * @param gc The render interface.
     * @param zval The z-value.
     */
    void
    selectColorForRendering(RenderInterface& gc, double zval, double edgeAlpha);
    //=============================================================================
    /**
     * @brief Gets a coordinate matrix.
     *
     * @param name The name of the matrix.
     * @param isXcoord Whether it's an x-coordinate matrix.
     * @return The coordinate matrix.
     */
    ArrayOf
    generateDataArray(const std::wstring& name, bool isXcoord);
    //=============================================================================
    /**
     * @brief Rebuilds the contour matrix.
     */
    void
    rebuildContourMatrix();
    //=============================================================================
    /**
     * @brief Gets the limits of the contour.
     *
     * @return A vector containing the limits.
     */
    std::vector<double>
    getLimits();
    //=============================================================================
    /**
     * @brief Driver function for contour generation.
     *
     * @param m The input matrix.
     * @param val The value for contour.
     * @param x The x-coordinate matrix.
     * @param y The y-coordinate matrix.
     * @return The generated contour lines.
     */
    contourLineSet
    contourGeneratorDriver(ArrayOf m, double val, const ArrayOf& x, const ArrayOf& y);
    //=============================================================================
    /**
     * @brief Adjusts the contour lines.
     *
     * @param bundledLines The set of bundled contour lines.
     * @param x The x-coordinate matrix.
     * @param y The y-coordinate matrix.
     * @param numx The number of x coordinates.
     * @param numy The number of y coordinates.
     * @return The adjusted contour lines.
     */
    contourLineSet
    adjustLines(
        contourLineSet& bundledLines, const ArrayOf& x, const ArrayOf& y, int numx, int numy);
    //=============================================================================
    /**
     * @brief Bundles the contour lines.
     *
     * @param allLines All the contour lines to be bundled.
     * @return The bundled contour lines.
     */
    contourLineSet
    bundleLines(contourLineSet& allLines);
    //=============================================================================
    /**
     * @brief Generates all the contour lines.
     *
     * @param m The input matrix.
     * @param val The value for contour.
     * @return All the generated contour lines.
     */
    contourLineSet
    generateAllLines(ArrayOf m, double val);
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
