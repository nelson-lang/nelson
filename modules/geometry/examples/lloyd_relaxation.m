%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Relax random seeds into an even mosaic with repeated Voronoi + centroid steps.
if ~exist('lloydFigureVisible', 'var')
  lloydFigureVisible = 'on';
end
if ~exist('lloydAnimationEnabled', 'var')
  lloydAnimationEnabled = true;
end
if ~exist('lloydAnimationFrames', 'var')
  lloydAnimationFrames = 160;
end
if ~exist('lloydFramePause', 'var')
  lloydFramePause = 0;
end
if ~exist('lloydAnimationContinuous', 'var')
  lloydAnimationContinuous = strcmp(lloydFigureVisible, 'on');
end
if ~exist('lloydPointCount', 'var')
  lloydPointCount = 80;
end
if ~exist('lloydSeed', 'var')
  lloydSeed = 3;
end
rng(lloydSeed);

lloydSeeds = rand(lloydPointCount, 2);
lloydColors = lloydCellColors(lloydPointCount);

lloydFigure = figure('Name', 'Lloyd relaxation', 'NumberTitle', 'off', ...
  'Color', [0.05 0.06 0.09], 'Visible', lloydFigureVisible, ...
  'Position', [140 70 820 830]);
lloydAxes = axes('Parent', lloydFigure, 'Position', [0.04 0.08 0.92 0.86], ...
  'Color', [0.05 0.06 0.09]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(lloydAxes);
hold(lloydAxes, 'on');
lloydPatches = gobjects(1, lloydPointCount);
for cellIndex = 1:lloydPointCount
  lloydPatches(cellIndex) = patch('Parent', lloydAxes, 'XData', nan, ...
    'YData', nan, 'FaceColor', lloydColors(cellIndex, :), ...
    'EdgeColor', [0.10 0.12 0.16], 'LineWidth', 1.0, 'FaceAlpha', 0.92);
end
lloydSeedScatter = plot(lloydAxes, lloydSeeds(:, 1), lloydSeeds(:, 2), ...
  '.', 'Color', [0.98 0.98 1.00], 'MarkerSize', 8, 'LineStyle', 'none');
hold(lloydAxes, 'off');
axis(lloydAxes, 'equal');
axis(lloydAxes, 'off');
set(lloydAxes, 'XLim', [0 1], 'YLim', [0 1]);
lloydTitle = title(lloydAxes, 'Lloyd relaxation toward a centroidal mosaic', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
lloydInfoText = uicontrol('Parent', lloydFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 780 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.09], 'ForegroundColor', [0.74 0.82 0.95], ...
  'FontSize', 10);

if lloydAnimationEnabled
  lloydFrame = 0;
  while isgraphics(lloydFigure)
    lloydFrame = lloydFrame + 1;
    if lloydFrame > lloydAnimationFrames && ~lloydAnimationContinuous
      break
    end
    if ~isgraphics(lloydFigure)
      break
    end
    try
      lloydClock = tic();
      [lloydSeeds, lloydShift] = lloydStep(lloydSeeds, lloydPatches);
      set(lloydSeedScatter, 'XData', lloydSeeds(:, 1), ...
        'YData', lloydSeeds(:, 2));
      lloydElapsed = toc(lloydClock);
      if isgraphics(lloydInfoText)
        set(lloydInfoText, 'String', sprintf(...
          '%d cells | mean move %.4f | %.0f relaxations/s', ...
          lloydPointCount, lloydShift, 1 / max(lloydElapsed, 1e-6)));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if lloydFramePause > 0
      pause(lloydFramePause);
    end
  end
end
%=============================================================================
function [seeds, meanShift] = lloydStep(seeds, patches)
  pointCount = size(seeds, 1);
  % Mirror the seeds across the four unit-square edges so every real cell is
  % bounded and clipped exactly to the box.
  mirrored = [seeds; ...
    [-seeds(:, 1) seeds(:, 2)]; [2 - seeds(:, 1) seeds(:, 2)]; ...
    [seeds(:, 1) -seeds(:, 2)]; [seeds(:, 1) 2 - seeds(:, 2)]];
  [vertices, cells] = voronoin(mirrored);
  newSeeds = seeds;
  totalShift = 0;
  for cellIndex = 1:pointCount
    vertexIndices = cells{cellIndex};
    if any(vertexIndices == 1)
      set(patches(cellIndex), 'XData', nan, 'YData', nan);
      continue
    end
    polygon = vertices(vertexIndices, :);
    polygon = lloydSortVertices(polygon);
    centroid = lloydPolygonCentroid(polygon);
    totalShift = totalShift + norm(centroid - seeds(cellIndex, :));
    newSeeds(cellIndex, :) = centroid;
    set(patches(cellIndex), 'XData', [polygon(:, 1); polygon(1, 1)], ...
      'YData', [polygon(:, 2); polygon(1, 2)]);
  end
  newSeeds = min(max(newSeeds, 0.001), 0.999);
  seeds = newSeeds;
  meanShift = totalShift / pointCount;
end
%=============================================================================
function ordered = lloydSortVertices(polygon)
  % Order the convex-cell vertices counterclockwise around their mean.
  center = mean(polygon, 1);
  angles = atan2(polygon(:, 2) - center(2), polygon(:, 1) - center(1));
  [~, order] = sort(angles);
  ordered = polygon(order, :);
end
%=============================================================================
function centroid = lloydPolygonCentroid(polygon)
  x = polygon(:, 1);
  y = polygon(:, 2);
  xNext = [x(2:end); x(1)];
  yNext = [y(2:end); y(1)];
  cross = x .* yNext - xNext .* y;
  area = sum(cross) / 2;
  if abs(area) < 1e-12
    centroid = mean(polygon, 1);
    return
  end
  cx = sum((x + xNext) .* cross) / (6 * area);
  cy = sum((y + yNext) .* cross) / (6 * area);
  centroid = [cx cy];
end
%=============================================================================
function colors = lloydCellColors(count)
  % A smooth spread of calm pastel hues, one per cell.
  hues = linspace(0, 0.85, count).';
  colors = zeros(count, 3);
  for index = 1:count
    colors(index, :) = lloydHsv(hues(index), 0.45, 0.95);
  end
end
%=============================================================================
function rgb = lloydHsv(hue, saturation, value)
  sector = floor(hue * 6);
  fraction = hue * 6 - sector;
  p = value * (1 - saturation);
  q = value * (1 - saturation * fraction);
  t = value * (1 - saturation * (1 - fraction));
  switch mod(sector, 6)
    case 0
      rgb = [value t p];
    case 1
      rgb = [q value p];
    case 2
      rgb = [p value t];
    case 3
      rgb = [p q value];
    case 4
      rgb = [t p value];
    otherwise
      rgb = [value p q];
  end
end
%=============================================================================
