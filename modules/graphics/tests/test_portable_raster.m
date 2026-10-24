%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testDirectory = fileparts(mfilename('fullpath'));
baselineFile = [testDirectory, '/fixtures/portable_raster_baselines.json'];
baseline = jsondecode(fileread(baselineFile));

asserts.isequal(baseline.schemaVersion, 1);
asserts.isequal(baseline.grid(:).', [6 8]);

[actual.line, pngBase64] = capture_line_scene();
actual.bars = capture_bar_scene();
actual.multi2d = capture_multi2d_scene();
actual.imageContour = capture_image_contour_scene();
actual.annotations = capture_annotations_scene();
actual.surface = capture_surface_scene();

asserts.istrue(startsWith(pngBase64, 'iVBORw0KGgo'));
asserts.istrue(length(pngBase64) > 500);

sceneNames = {'line', 'bars', 'multi2d', 'imageContour', 'annotations', 'surface'};
for sceneIndex = 1:length(sceneNames)
  sceneName = sceneNames{sceneIndex};
  compare_raster_fingerprint(actual.(sceneName), baseline.scenes.(sceneName), sceneName);
end

function [result, pngBase64] = capture_line_scene()
  f = figure('Visible', 'off', 'Position', [0 0 160 120]);
  cleanup = onCleanup(@() close(f)); %#ok<NASGU>
  plot([0 1 2 3 4], [0.1 0.8 0.25 0.9 0.4], ...
    'Color', [0.85 0.1 0.1], 'LineWidth', 3);
  xlim([0 4]);
  ylim([0 1]);
  grid('on');
  frame = getframe(f);
  result = raster_fingerprint(frame.cdata, 6, 8);
  pngBase64 = __web_raster_png__(f);
end

function result = capture_bar_scene()
  f = figure('Visible', 'off', 'Position', [0 0 160 120]);
  cleanup = onCleanup(@() close(f)); %#ok<NASGU>
  h = bar([1 3 2 4]);
  h.FaceColor = [0.15 0.45 0.8];
  ylim([0 5]);
  result = raster_fingerprint(getframe(f).cdata, 6, 8);
end

function result = capture_multi2d_scene()
  f = figure('Visible', 'off', 'Position', [0 0 240 180]);
  cleanup = onCleanup(@() close(f)); %#ok<NASGU>
  layout = tiledlayout(f, 2, 2, 'TileSpacing', 'compact', 'Padding', 'compact');
  ax = nexttile(layout);
  scatter(ax, [1 2 3 4], [4 1 3 2], 28, [0.8 0.2 0.2], 'filled');
  ax = nexttile(layout);
  stairs(ax, 0:4, [0 1 0.5 1.5 1], 'LineWidth', 2);
  ax = nexttile(layout);
  stem(ax, 1:4, [1 3 2 4]);
  ax = nexttile(layout);
  area(ax, 1:4, [1 2 1 3], 'FaceColor', [0.2 0.65 0.35]);
  result = raster_fingerprint(getframe(f).cdata, 6, 8);
end

function result = capture_image_contour_scene()
  f = figure('Visible', 'off', 'Position', [0 0 240 140]);
  cleanup = onCleanup(@() close(f)); %#ok<NASGU>
  subplot(1, 2, 1);
  imagesc(peaks(12));
  subplot(1, 2, 2);
  contour(peaks(12), 7, 'LineWidth', 1.5);
  result = raster_fingerprint(getframe(f).cdata, 6, 8);
end

function result = capture_annotations_scene()
  f = figure('Visible', 'off', 'Position', [0 0 200 150]);
  cleanup = onCleanup(@() close(f)); %#ok<NASGU>
  semilogy(1:4, [1 10 100 1000], '-o', 'LineWidth', 2, ...
    'DisplayName', 'growth');
  title('Portable annotations');
  xlabel('sample');
  ylabel('value');
  legend('show');
  grid('on');
  result = raster_fingerprint(getframe(f).cdata, 6, 8);
end

function result = capture_surface_scene()
  f = figure('Visible', 'off', 'Position', [0 0 160 120]);
  cleanup = onCleanup(@() close(f)); %#ok<NASGU>
  surf(peaks(15));
  view(45, 25);
  result = raster_fingerprint(getframe(f).cdata, 6, 8);
end

function result = raster_fingerprint(image, rowCount, columnCount)
  height = size(image, 1);
  width = size(image, 2);
  rowEdges = round(linspace(1, height + 1, rowCount + 1));
  columnEdges = round(linspace(1, width + 1, columnCount + 1));
  samples = zeros(rowCount * columnCount, 3);
  sampleIndex = 0;
  for rowIndex = 1:rowCount
    rows = rowEdges(rowIndex):(rowEdges(rowIndex + 1) - 1);
    for columnIndex = 1:columnCount
      sampleIndex = sampleIndex + 1;
      columns = columnEdges(columnIndex):(columnEdges(columnIndex + 1) - 1);
      for channel = 1:3
        block = double(image(rows, columns, channel));
        samples(sampleIndex, channel) = sum(block(:)) / numel(block);
      end
    end
  end
  result = struct('size', [height width 3], 'samples', samples);
end

function compare_raster_fingerprint(actual, expected, sceneName)
  asserts.isequal(actual.size, expected.size(:).');
  asserts.isequal(size(actual.samples), size(expected.samples));
  difference = abs(actual.samples - expected.samples);
  meanAbsoluteError = sum(difference(:)) / numel(difference);
  outlierFraction = sum(difference(:) > 20) / numel(difference);
  if meanAbsoluteError > 5
    error('Nelson:graphics:rasterBaselineMean', ...
      [sceneName, ' raster mean absolute error: ', num2str(meanAbsoluteError)]);
  end
  if outlierFraction > 0.10
    error('Nelson:graphics:rasterBaselineOutliers', ...
      [sceneName, ' raster outlier fraction: ', num2str(outlierFraction)]);
  end
end
%=============================================================================
