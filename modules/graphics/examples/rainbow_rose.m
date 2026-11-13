%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Render a parametric rose surface.
% The petals oscillate 3.6 times per turn over ten turns: sample the angle finely.
radialCoordinates = linspace(0, 1, 30);
angularCoordinates = linspace(-2, 20 * pi, 1200);
[radiusGrid, angleGrid] = meshgrid(radialCoordinates, angularCoordinates);

petalShape = 1 - 0.5 * ((5 / 4) * ...
  (1 - mod(3.6 * angleGrid, 2 * pi) / pi) .^ 2 - 0.25) .^ 2;
decay = 2 * exp(-angleGrid / (8 * pi));
decaySine = sin(decay);
decayCosine = cos(decay);
radialModulation = 1.99 * radiusGrid .^ 2 .* ...
  (1.2 * radiusGrid - 1) .^ 2 .* decaySine;
surfaceRadius = petalShape .* ...
  (radiusGrid .* decaySine + radialModulation .* decayCosine);

x = surfaceRadius .* sin(angleGrid);
y = surfaceRadius .* cos(angleGrid);
z = petalShape .* ...
  (radiusGrid .* decayCosine - radialModulation .* decaySine);

figureHandle = figure('SelectionHighlight', 'off', 'Name', 'Parametric rainbow rose', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
surf(axesHandle, x, y, z, angleGrid, 'LineStyle', 'none', 'FaceLighting', 'gouraud');
axis(axesHandle, 'equal');
axis(axesHandle, 'off');
colormap(axesHandle, jet(128));
view(axesHandle, 3);
light(axesHandle, 'Position', [-2 -2 4]);
title(axesHandle, 'Parametric rainbow rose', 'Visible', 'on');
%=============================================================================
