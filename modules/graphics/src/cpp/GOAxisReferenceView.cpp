//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOAxis.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
GOAxis::navigationReferenceView(bool replace)
{
    if (replace || m_navigationReferenceView.isEmpty()) {
        updateStateIfDirty();
        const wstringVector names = { L"View", L"Projection", L"XScale", L"YScale", L"ZScale",
            L"XDir", L"YDir", L"ZDir", L"XLim", L"YLim", L"ZLim", L"DataAspectRatio",
            L"PlotBoxAspectRatio", L"CameraPosition", L"CameraTarget", L"CameraUpVector",
            L"CameraViewAngle", L"XLimMode", L"YLimMode", L"ZLimMode", L"DataAspectRatioMode",
            L"PlotBoxAspectRatioMode", L"CameraPositionMode", L"CameraTargetMode",
            L"CameraUpVectorMode", L"CameraViewAngleMode" };
        ArrayOfVector values;
        for (const auto& name : names) {
            values.push_back(findProperty(name)->get());
        }
        m_navigationReferenceView = ArrayOf::structScalar(names, values);
    }
    return m_navigationReferenceView;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
