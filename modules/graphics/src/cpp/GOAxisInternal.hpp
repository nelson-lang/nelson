//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
// Shared numeric constants for the GOAxis translation units. Extracted from
// GOAxis.cpp so that split-out TUs (GOAxis_*.cpp) can reuse them without
// duplicating the values. inline constexpr keeps a single definition (no ODR issue).
//=============================================================================
namespace Nelson {
//=============================================================================
inline constexpr double DEFAULT_CAMERA_VIEW_ANGLE = 6.60861036031192;
inline constexpr double PROJECTION_CLIP_MARGIN = 0.01;
inline constexpr double DEPTH_PROJECTION_CLIP_EXPANSION = 100.0;
inline constexpr double DEFAULT_3D_PLOT_BOX_ASPECT_MIN = 0.78023598820059;
inline constexpr double DEFAULT_3D_PLOT_BOX_ASPECT_Z_MAX = 0.879058613749489;
inline constexpr double DEFAULT_3D_PLOT_BOX_ASPECT_XY_POWER = 1.7056592968232565;
inline constexpr double DEFAULT_3D_PLOT_BOX_ASPECT_Z_POWER = 1.9;
inline constexpr double UI_AXES_LEFT_INSET = 10.9333329518636;
inline constexpr double UI_AXES_BOTTOM_INSET = 17.3333327412605;
inline constexpr double UI_AXES_RIGHT_INSET = 3.60000006357827;
inline constexpr double UI_AXES_TOP_INSET = 17.7000002543131;
inline constexpr double UI_AXES_PROPERTY_LEFT_INSET = 20.1333338419596;
inline constexpr double UI_AXES_PROPERTY_BOTTOM_INSET = 16.9333327611287;
inline constexpr double UI_AXES_PROPERTY_RIGHT_INSET = 5.0;
inline constexpr double UI_AXES_PROPERTY_TOP_INSET = 6.8;
inline constexpr double AUTO_FONT_FULL_WIDTH = 299.96;
inline constexpr double AUTO_FONT_MIN_WIDTH = 106.08;
inline constexpr double AUTO_FONT_HEIGHT_TO_WIDTH = 1.2495;
inline constexpr double AUTO_FONT_MIN_SCALE = 0.8;
inline constexpr double AXES_FACTORY_FONT_SIZE = 10.0;
inline constexpr double DEFAULT_PLOT_BOX_WIDTH = 0.775;
inline constexpr double DEFAULT_PLOT_BOX_HEIGHT = 0.815;
inline constexpr double POLAR_THETA_LABEL_GAP_BASE = 3.3;
inline constexpr double POLAR_THETA_LABEL_GAP_EM = 0.24;
//=============================================================================
} // namespace Nelson
//=============================================================================
