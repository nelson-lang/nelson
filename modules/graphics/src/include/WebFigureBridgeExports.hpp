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
#include <cstdint>
#include "nlsGraphics_exports.h"
//=============================================================================
// C entry points implemented in WebFigure*.cpp and consumed from several
// translation units (DrawNow, HeadlessFigureHost, __view__Builtin, and the
// web_gui bridge via GetProcAddress). Keep the declarations here so a signature
// change is caught at compile time instead of at link (or run) time.
extern "C"
{
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiPickDataPoint(
        int64_t handle, double x, double y, void (*sink)(void*, const char*, int), void* context);
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiInspectDataPoint(int64_t handle, int64_t object, double sample,
        void (*sink)(void*, const char*, int), void* context);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiCloseFigure(int64_t handle);
    //=============================================================================
    NLSGRAPHICS_IMPEXP void
    nlsWebGuiClearCurrentFigure();
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureDisplayList(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx);
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureDisplayListNoFallback(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureAnimationFrame(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx);
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureInteractiveFrame(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigureAnimationFrameWithAttachments(int64_t handle,
        void (*sink)(void*, const char*, int), void* ctx,
        void (*attachmentSink)(void*, const char*, const unsigned char*, int), void* attachmentCtx);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigurePngBase64(
        int64_t handle, void (*sink)(void*, const char*, int), void* ctx);
    //=============================================================================
    NLSGRAPHICS_IMPEXP void
    nlsWebGuiMarkFigureViewOnlyFrame(int64_t handle);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiSetAxesView(int64_t handle, int64_t axesHandle, double azimuth, double elevation);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiPanAxes(int64_t handle, double dxFrac, double dyFrac);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiSetFigureSize(int64_t handle, int width, int height);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiSetFigurePosition(int64_t handle, int x, int y, int width, int height);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiFigureKeyEvent(int64_t handle, bool pressed, const char* character, const char* key,
        bool shift, bool control, bool alt, bool meta);
    //=============================================================================
    // action: "press", "motion" or "release"; (x, y) in pixels of the web
    // surface, bottom-left origin. *listensToMotion tells the client whether
    // motion events are worth sending (WindowButtonMotionFcn set). button is 0
    // (left), 1 (middle) or 2 (right); the modifiers and doubleClick set the
    // figure SelectionType on a "press".
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiFigureMouseEvent(int64_t handle, const char* action, double x, double y,
        bool* listensToMotion, int button, bool shiftDown, bool ctrlDown, bool doubleClick);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiUiHtmlPull(int64_t handle, void (*sink)(void*, const char*, int), void* ctx);
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiUIControlAction(int64_t handle, double value, const char* text);
    //=============================================================================
}
//=============================================================================
