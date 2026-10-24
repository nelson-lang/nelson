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
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fsurf(ax, @(x, y) sin(x) + cos(y), [-2 2 -3 3], 'MeshDensity', 17, 'FaceColor', 'interp');
asserts.istrue(isgraphics(h, 'functionsurface'));
asserts.isequal(h.Type, 'functionsurface');
asserts.isequal(h.Parent, ax);
asserts.isequal(h.MeshDensity, 17);
asserts.isequal(h.XRange, [-2 2]);
asserts.isequal(h.YRange, [-3 3]);
asserts.isequal(ax.XLim, [-2 2]);
asserts.isequal(ax.YLim, [-3 3]);
asserts.isequal(h.XRangeMode, 'manual');
asserts.isequal(h.YRangeMode, 'manual');
asserts.isequal(size(h.XData), [17 17]);
asserts.isequal(size(h.YData), [17 17]);
asserts.isequal(size(h.ZData), [17 17]);
asserts.isequal(h.FaceColor, 'interp');
%=============================================================================
