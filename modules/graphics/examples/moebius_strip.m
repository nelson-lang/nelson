%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Draw a one-sided Möbius strip from its parametric equations.
angle = linspace(0, 2 * pi, 180);
width = linspace(-0.45, 0.45, 28);
[angleGrid, widthGrid] = meshgrid(angle, width);
radius = 1 + 0.5 * widthGrid .* cos(angleGrid / 2);
x = radius .* cos(angleGrid);
y = radius .* sin(angleGrid);
z = 0.5 * widthGrid .* sin(angleGrid / 2);

figureHandle = figure('Name', 'Moebius strip', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
surfaceHandle = surf(axesHandle, x, y, z, angleGrid);
set(surfaceHandle, 'EdgeColor', 'none');
axis(axesHandle, 'equal');
axis(axesHandle, 'off');
colormap(axesHandle, parula(256));
view(axesHandle, 3);
title(axesHandle, 'A one-sided Moebius strip');
%=============================================================================
