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
pointCount = 80;
radialCoordinates = linspace(0, 1, pointCount);
angularCoordinates = linspace(-2, 20 * pi, pointCount);
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

figureHandle = figure('Name', 'Parametric rainbow rose', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
surf(axesHandle, x, y, z, angleGrid, 'LineStyle', 'none');
axis(axesHandle, 'equal');
axis(axesHandle, 'off');
colormap(axesHandle, jet(128));
view(axesHandle, 3);
title(axesHandle, 'Parametric rainbow rose', 'Visible', 'on');
%=============================================================================
