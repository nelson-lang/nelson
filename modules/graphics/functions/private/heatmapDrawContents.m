%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Draws (or redraws) the internal primitives of a heatmap chart into its group
% from colorData + opts. Deletes any primitives from a previous draw so a
% property change can rebuild the chart in place. Returns the state struct it
% also stores in group.UserData.
function state = heatmapDrawContents(ax, group, colorData, xLabels, yLabels, opts)
  deletePreviousContents(group);
  [rows, cols] = size(colorData);
  imageArgs = {};
  if isfield(opts, 'ImageArgs') && ~isempty(opts.ImageArgs)
    imageArgs = opts.ImageArgs;
  end
  applyHeatmapColorScale(ax, opts);
  % The chart draws its image, grid, missing-data overlay and cell labels as
  % internal primitives with HandleVisibility 'off', so findobj / Children do
  % not expose them while they still render and stay reachable through UserData.
  imageHandle = __image__('Parent', group, 'CData', colorData, ...
    'XData', [1, max(cols, 1)], 'YData', [1, max(rows, 1)], ...
    'CDataMapping', 'scaled', 'HandleVisibility', 'off', imageArgs{:});
  applyAxesState(ax, rows, cols, xLabels, yLabels, opts);
  missingHandles = drawMissingDataCells(group, colorData, opts);
  gridHandles = drawHeatmapGrid(group, rows, cols, opts);
  labelHandles = drawCellLabels(ax, group, colorData, opts);
  colorbarHandle = createHeatmapColorbar(ax, opts);
  applyTextLabels(ax, opts);

  state = struct();
  state.ChartType = 'heatmap';
  state.Image = imageHandle;
  state.MissingCells = missingHandles;
  state.Grid = gridHandles;
  state.CellLabels = labelHandles;
  state.Colorbar = colorbarHandle;
  state.XData = xLabels;
  state.YData = yLabels;
  state.ColorData = colorData;
  state.SourceTable = opts.SourceTable;
  state.XVariable = opts.XVariable;
  state.YVariable = opts.YVariable;
  state.ColorVariable = opts.ColorVariable;
  state.ColorMethod = opts.ColorMethod;
  state.Options = opts;
  group.UserData = state;
end
%=============================================================================
function deletePreviousContents(group)
  state = group.UserData;
  if ~isstruct(state) || ~isfield(state, 'ChartType')
    return
  end
  fields = {'Image', 'MissingCells', 'Grid', 'CellLabels', 'Colorbar'};
  for k = 1:numel(fields)
    if isfield(state, fields{k})
      deleteHandles(state.(fields{k}));
    end
  end
end
%=============================================================================
function deleteHandles(handles)
  for k = 1:numel(handles)
    h = handles(k);
    if isgraphics(h)
      delete(h);
    end
  end
end
%=============================================================================
function applyAxesState(ax, rows, cols, xLabels, yLabels, opts)
  ax.YDir = 'reverse';
  ax.XLim = [0.5, cols + 0.5];
  ax.YLim = [0.5, rows + 0.5];
  ax.XTick = 1:cols;
  ax.YTick = 1:rows;
  ax.XTickLabel = xLabels;
  ax.YTickLabel = yLabels;
  ax.TickLength = [0 0];
  ax.Layer = 'top';
  ax.Box = 'on';
  ax.Visible = opts.Visible;
end
%=============================================================================
function handles = drawMissingDataCells(parent, colorData, opts)
  handles = [];
  [rows, cols] = size(colorData);
  mask = isnan(colorData);
  if ~any(mask(:))
    return
  end
  rgb = zeros(rows, cols, 3);
  rgb(:, :, 1) = opts.MissingDataColor(1);
  rgb(:, :, 2) = opts.MissingDataColor(2);
  rgb(:, :, 3) = opts.MissingDataColor(3);
  handles = __image__('Parent', parent, 'CData', rgb, ...
    'XData', [1, max(cols, 1)], 'YData', [1, max(rows, 1)], ...
    'CDataMapping', 'direct', 'AlphaData', double(mask), 'AlphaDataMapping', 'none', ...
    'HandleVisibility', 'off');
end
%=============================================================================
function handles = drawHeatmapGrid(parent, rows, cols, opts)
  handles = [];
  if strcmp(opts.GridVisible, 'off')
    return
  end
  color = graphicsDefaultEdgeColor();
  % One line object holds the whole grid: NaN breaks separate the segments.
  edgesX = (0:cols) + 0.5;
  edgesY = (0:rows) + 0.5;
  verticalX = [edgesX; edgesX; NaN(1, cols + 1)];
  verticalY = repmat([0.5; rows + 0.5; NaN], 1, cols + 1);
  horizontalX = repmat([0.5; cols + 0.5; NaN], 1, rows + 1);
  horizontalY = [edgesY; edgesY; NaN(1, rows + 1)];
  handles = line('Parent', parent, ...
    'XData', [verticalX(:); horizontalX(:)], 'YData', [verticalY(:); horizontalY(:)], ...
    'Color', color, 'LineWidth', 0.5, 'HandleVisibility', 'off');
end
%=============================================================================
function handles = drawCellLabels(ax, parent, colorData, opts)
  [rows, cols] = size(colorData);
  handles = [];
  if ~cellLabelsFit(ax, colorData, opts)
    return
  end
  collected = cell(1, rows * cols);
  for i = 1:rows
    for j = 1:cols
      value = colorData(i, j);
      if isnan(value)
        label = opts.MissingDataLabel;
      else
        label = sprintf(opts.CellLabelFormat, value);
      end
      labelColor = cellLabelColor(value, opts);
      collected{(i - 1) * cols + j} = __text__('Parent', parent, 'Position', [j i 0], ...
        'String', label, 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', ...
        'FontSize', opts.FontSize, 'Color', labelColor, 'HandleVisibility', 'off');
    end
  end
  handles = [collected{:}];
end
%=============================================================================
% Labels that cannot be read in their cell are not created (the reference hides them too), which
% keeps large heatmaps from creating one text object per cell.
function fits = cellLabelsFit(ax, colorData, opts)
  fits = true;
  [rows, cols] = size(colorData);
  if rows * cols < 100
    return
  end
  pixels = axesPixelSize(ax);
  if any(~isfinite(pixels))
    return
  end
  fontPixels = double(opts.FontSize) * 96 / 72;
  finiteValues = abs(colorData(isfinite(colorData)));
  if isempty(finiteValues)
    finiteValues = 0;
  end
  sample = sprintf(opts.CellLabelFormat, max(finiteValues(:)));
  textWidth = max(numel(sample), numel(opts.MissingDataLabel)) * 0.3 * fontPixels;
  fits = pixels(1) / cols >= textWidth && pixels(2) / rows >= 0.9 * fontPixels;
end
%=============================================================================
function pixels = axesPixelSize(ax)
  pixels = [Inf Inf];
  position = get(ax, 'Position');
  switch get(ax, 'Units')
    case 'pixels'
      pixels = position(3:4);
    case 'normalized'
      figureHandle = ancestor(ax, 'figure');
      if ~isempty(figureHandle) && strcmp(get(figureHandle, 'Units'), 'pixels')
        figurePosition = get(figureHandle, 'Position');
        pixels = position(3:4) .* figurePosition(3:4);
      end
  end
end
%=============================================================================
function color = cellLabelColor(value, opts)
  color = opts.CellLabelColor;
  if ~(ischar(color) && strcmp(color, 'auto'))
    return
  end
  color = opts.FontColor;
  if isnan(value) || isempty(opts.Colormap) || isempty(opts.ColorLimits)
    return
  end
  limits = opts.ColorLimits;
  position = (value - limits(1)) / (limits(2) - limits(1));
  position = max(0, min(1, position));
  index = round(position * (size(opts.Colormap, 1) - 1)) + 1;
  background = opts.Colormap(index, :);
  luminance = 0.2126 * background(1) + 0.7152 * background(2) + 0.0722 * background(3);
  if luminance < 0.5
    color = [1 1 1];
  end
end
%=============================================================================
function applyHeatmapColorScale(ax, opts)
  if ~isempty(opts.ColorLimits)
    clim(ax, opts.ColorLimits);
  end
  if ~isempty(opts.Colormap)
    colormap(ax, opts.Colormap);
  end
end
%=============================================================================
function colorbarHandle = createHeatmapColorbar(ax, opts)
  colorbarHandle = [];
  if strcmp(opts.ColorbarVisible, 'on')
    colorbarHandle = colorbar(ax);
  end
end
%=============================================================================
function applyTextLabels(ax, opts)
  if ~isempty(opts.Title)
    title(ax, opts.Title);
  end
  if ~isempty(opts.XLabel)
    xlabel(ax, opts.XLabel);
  end
  if ~isempty(opts.YLabel)
    ylabel(ax, opts.YLabel);
  end
end
%=============================================================================
