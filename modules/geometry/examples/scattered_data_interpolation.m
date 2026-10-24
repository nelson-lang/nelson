%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Triangulate scattered samples, build their Voronoi diagram and interpolate.
samplePoints = [...
  -1.00, -1.00; -0.50, -1.00; 0.00, -1.00; 0.50, -1.00; 1.00, -1.00; ...
  -1.00, -0.50; -0.55, -0.35; 0.05, -0.55; 0.55, -0.35; 1.00, -0.50; ...
  -1.00, 0.00; -0.35, 0.05; 0.25, 0.15; 0.70, 0.05; 1.00, 0.00; ...
  -1.00, 0.50; -0.60, 0.65; -0.05, 0.45; 0.45, 0.70; 1.00, 0.50; ...
  -1.00, 1.00; -0.50, 1.00; 0.00, 1.00; 0.50, 1.00; 1.00, 1.00];
sampleValues = sin(pi * samplePoints(:, 1)) .* cos(pi * samplePoints(:, 2)) ...
  + 0.2 * samplePoints(:, 1);
triangles = delaunay(samplePoints);
[voronoiX, voronoiY] = voronoi(samplePoints);
[queryX, queryY] = meshgrid(linspace(-1, 1, 45));
interpolatedValues = griddata(...
  samplePoints, sampleValues, queryX, queryY, 'linear');

if ~exist('scatteredDataFigureVisible', 'var')
  scatteredDataFigureVisible = 'on';
end
figureHandle = figure(...
  'Name', 'Scattered data interpolation', ...
  'NumberTitle', 'off', ...
  'Visible', scatteredDataFigureVisible);

triangulationAxes = subplot(1, 3, 1, 'Parent', figureHandle);
axes(triangulationAxes);
triplot(triangles, samplePoints(:, 1), samplePoints(:, 2), ...
  'Color', [0.25 0.45 0.75]);
hold(triangulationAxes, 'on');
scatter(triangulationAxes, samplePoints(:, 1), samplePoints(:, 2), ...
  28, sampleValues, 'filled');
hold(triangulationAxes, 'off');
axis(triangulationAxes, 'equal');
title(triangulationAxes, 'Delaunay triangulation');

voronoiAxes = subplot(1, 3, 2, 'Parent', figureHandle);
plot(voronoiAxes, voronoiX, voronoiY, '-', ...
  'Color', [0.20 0.55 0.35], 'LineWidth', 1.1);
hold(voronoiAxes, 'on');
plot(voronoiAxes, samplePoints(:, 1), samplePoints(:, 2), 'k.');
hold(voronoiAxes, 'off');
axis(voronoiAxes, 'equal');
axis(voronoiAxes, [-1.1 1.1 -1.1 1.1]);
title(voronoiAxes, 'Voronoi cells');

interpolationAxes = subplot(1, 3, 3, 'Parent', figureHandle);
surf(interpolationAxes, queryX, queryY, interpolatedValues, ...
  'EdgeColor', 'none');
hold(interpolationAxes, 'on');
plot3(interpolationAxes, samplePoints(:, 1), samplePoints(:, 2), ...
  sampleValues, 'k.', 'MarkerSize', 10);
hold(interpolationAxes, 'off');
view(interpolationAxes, 3);
grid(interpolationAxes, 'on');
title(interpolationAxes, 'Linear interpolation');
colormap(figureHandle, parula(128));
%=============================================================================
