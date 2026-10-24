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
#include <string>
#include <vector>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOFigure;
//=============================================================================
enum class MOUSE_MODE
{
    NONE = 0,
    ROTATION,
    ZOOM_IN,
    ZOOM_OUT,
    PAN,
};
//=============================================================================
enum class WAIT_PRESS_MODE
{
    NONE = 0,
    MOUSE,
    KEY,
    CLOSE,
    WAIT
};
//=============================================================================
// Qt-free host of one figure.
//
// The drawing model talks to its window only through this interface, so it never
// references a windowing type. The Qt desktop implements it with GOWindow; WEB_GUI uses
// the headless implementation. The windowing layer registers a factory (see
// setFigureHostFactory) and calls into the model; the model never links back.
//=============================================================================
class GOFigureHost
{
public:
    virtual ~GOFigureHost() = default;
    //=============================================================================
    virtual GOFigure*
    hostFigure()
        = 0;
    virtual int64
    hostId() const
        = 0;
    virtual bool
    isHeadlessHost() const
        = 0;
    //=============================================================================
    virtual void
    requestHostRepaint()
        = 0;
    virtual void
    requestHostRepaintNow()
        = 0;
    virtual bool
    syncHostRendering(int timeoutMs)
        = 0;
    virtual void
    updateHostState(bool force)
        = 0;
    //=============================================================================
    virtual void
    presentHost()
        = 0;
    // Undocks (when forced) and hides, ahead of destruction.
    virtual void
    prepareHostClose(bool forceClose)
        = 0;
    // The model never calls `delete` on a host directly.
    virtual void
    destroyHost()
        = 0;
    virtual bool
    isDockedHost()
        = 0;
    //=============================================================================
    // Pixels. Bottom-up Y conversions stay in the model. Returns false when headless.
    virtual bool
    hostFrameGeometry(double& x, double& y, double& width, double& height, double& frameTop)
        = 0;
    //=============================================================================
    virtual void
    setWaitMouseOrKeyPressEvent(bool waiting)
        = 0;
    virtual bool
    isWaitingMouseOrKeyPressEvent(WAIT_PRESS_MODE& waitPressMode)
        = 0;
    //=============================================================================
    // Qt desktop path, false when headless. rgba is tightly-packed RGBA8, row-major.
    virtual bool
    grabHostFrameRGBA(bool rawOpenGL, std::vector<unsigned char>& rgba, int& width, int& height)
        = 0;
    //=============================================================================
    // Opaque native widget (QWidget*) or nullptr when headless. Only the Qt-side widget
    // layer casts it back.
    virtual void*
    nativeParentWidget()
        = 0;
    virtual void
    rebuildHostMenus()
        = 0;
    //=============================================================================
    virtual MOUSE_MODE
    currentHostMouseMode()
        = 0;
    virtual void
    toggleHostMouseMode(MOUSE_MODE mode)
        = 0;
    //=============================================================================
    // Test hook: replays a mouse event through the host input path. action is L"press",
    // L"motion" or L"release"; (x, y) are logical pixels from the bottom-left corner of
    // the drawing area. button is 0 (left), 1 (middle) or 2 (right).
    virtual bool
    injectHostMouseEvent(const std::wstring& action, double x, double y, int button = 0,
        bool shiftDown = false, bool ctrlDown = false, bool doubleClick = false)
        = 0;
    // Test hook: [left bottom width height] of the drawing area in logical pixels, bottom-up
    // from the primary screen. Returns false when headless or hidden.
    virtual bool
    hostDrawingAreaScreenRect(double& x, double& y, double& width, double& height)
        = 0;
};
//=============================================================================
// Registered by the windowing layer at load. `headless` mirrors WEB_GUI mode.
using FigureHostFactory = GOFigureHost* (*)(int64 figureId, bool headless);
//=============================================================================
} // namespace Nelson
//=============================================================================
