%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% zoom(fig) returns a zoom-mode object when an output is requested
f = figure('Visible', 'off');
plot3(1:10, 1:10, 1:10);
ax = gca();
z = zoom(f);
assert_isequal(class(z), 'ZoomObject');
assert_isequal(z.Figure, f);
%=============================================================================
% requesting the object does not alter the current limits (no toggle)
xl = ax.XLim;
yl = ax.YLim;
z2 = zoom(ax);
assert_isequal(ax.XLim, xl);
assert_isequal(ax.YLim, yl);
%=============================================================================
% setAxes3DPanAndZoomStyle stores the style on the axes
z.setAxes3DPanAndZoomStyle(ax, 'camera');
assert_isequal(getappdata(ax, 'Axes3DPanAndZoomStyle'), 'camera');
z.setAxes3DPanAndZoomStyle(ax, 'limits');
assert_isequal(getappdata(ax, 'Axes3DPanAndZoomStyle'), 'limits');
%=============================================================================
% style accepts string scalars and is case-insensitive
z.setAxes3DPanAndZoomStyle(ax, "CAMERA");
assert_isequal(getappdata(ax, 'Axes3DPanAndZoomStyle'), 'camera');
%=============================================================================
% an invalid style is rejected
cmd = 'z.setAxes3DPanAndZoomStyle(ax, ''wrong'')';
assert_checkerror(cmd, _('Style must be ''limits'' or ''camera''.'));
%=============================================================================
% the object exposes the usual zoom-mode properties
assert_istrue(isprop(z, 'Enable'));
assert_istrue(isprop(z, 'Motion'));
assert_istrue(isprop(z, 'Direction'));
close(f);
%=============================================================================
% an output can still be combined with an explicit action
f = figure('Visible', 'off');
plot(sin(0:0.1:2 * pi));
ax = gca();
zoom('reset');
initialXLim = ax.XLim;
z = zoom(ax, 'on');
assert_isequal(class(z), 'ZoomObject');
z = zoom(1.5);
assert_istrue(diff(ax.XLim) < diff(initialXLim));
assert_isequal(class(z), 'ZoomObject');
close(f);
%=============================================================================
