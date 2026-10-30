%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Store a synthetic temperature-anomaly field in NetCDF and play it back.
if ~exist('ncClimateFigureVisible', 'var')
  ncClimateFigureVisible = 'on';
end
if ~exist('ncClimateAnimationEnabled', 'var')
  ncClimateAnimationEnabled = true;
end
if ~exist('ncClimateFramePause', 'var')
  ncClimateFramePause = 0.04;
end
if ~exist('ncClimateAnimationContinuous', 'var')
  ncClimateAnimationContinuous = strcmp(ncClimateFigureVisible, 'on');
end
if ~exist('ncClimateMonths', 'var')
  ncClimateMonths = 72;
end

ncClimateLon = 96;
ncClimateLat = 48;
% Build a moving anomaly field, then write it to a NetCDF variable.
[lonGrid, latGrid] = meshgrid(linspace(0, 1, ncClimateLon), ...
  linspace(0, 1, ncClimateLat));
field = zeros(ncClimateLat, ncClimateLon, ncClimateMonths);
for month = 1:ncClimateMonths
  phase = month / ncClimateMonths * 2 * pi;
  warm = 1.2 * exp(-(((lonGrid - (0.3 + 0.2 * sin(phase))) .^ 2 + ...
    (latGrid - 0.4) .^ 2) / 0.02));
  cold = -1.0 * exp(-(((lonGrid - (0.7 + 0.15 * cos(phase))) .^ 2 + ...
    (latGrid - 0.6) .^ 2) / 0.03));
  season = 0.3 * sin(phase) * (latGrid - 0.5);
  field(:, :, month) = warm + cold + season;
end
ncClimateFile = [tempname(), '.nc'];
if isfile(ncClimateFile)
  delete(ncClimateFile);
end
nccreate(ncClimateFile, 'anomaly', 'Dimensions', ...
  {'lat', ncClimateLat, 'lon', ncClimateLon, 'month', ncClimateMonths});
ncwrite(ncClimateFile, 'anomaly', field);
% Read the field back from NetCDF: the file is the only data source now.
ncClimateData = ncread(ncClimateFile, 'anomaly');

ncClimateFigure = figure('Name', 'NetCDF climate field', 'NumberTitle', ...
  'off', 'Color', [0.05 0.06 0.09], 'Visible', ncClimateFigureVisible, ...
  'Position', [100 120 980 560]);
ncClimateAxes = axes('Parent', ncClimateFigure, 'Position', ...
  [0.04 0.08 0.92 0.82]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(ncClimateAxes);
ncClimateImage = image(ncClimateAxes, 'CData', ...
  ncClimateColor(ncClimateData(:, :, 1)));
axis(ncClimateAxes, 'image');
axis(ncClimateAxes, 'off');
set(ncClimateAxes, 'YDir', 'normal');
ncClimateTitle = title(ncClimateAxes, '', 'Color', [0.92 0.94 0.98], ...
  'FontSize', 13);
ncClimateInfo = uicontrol('Parent', ncClimateFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 900 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.09], 'ForegroundColor', [0.80 0.86 0.96], ...
  'FontSize', 10);

if ncClimateAnimationEnabled
  ncClimateFrame = 0;
  while isgraphics(ncClimateFigure)
    ncClimateFrame = ncClimateFrame + 1;
    month = mod(ncClimateFrame - 1, ncClimateMonths) + 1;
    if ~ncClimateAnimationContinuous && ncClimateFrame > ncClimateMonths
      break
    end
    if ~isgraphics(ncClimateFigure)
      break
    end
    try
      slice = ncClimateData(:, :, month);
      set(ncClimateImage, 'CData', ncClimateColor(slice));
      set(ncClimateTitle, 'String', sprintf(...
        'Temperature anomaly, month %d of %d', month, ncClimateMonths));
      if isgraphics(ncClimateInfo)
        set(ncClimateInfo, 'String', sprintf(...
          '%dx%d grid x %d months read from NetCDF | range %.2f to %.2f', ...
          ncClimateLon, ncClimateLat, ncClimateMonths, min(slice(:)), ...
          max(slice(:))));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if ncClimateFramePause > 0
      pause(ncClimateFramePause);
    end
  end
end
%=============================================================================
function rgb = ncClimateColor(field)
  % Diverging blue-white-red map: blue is cold, red is warm.
  value = max(min(field, 1), -1);
  positive = max(value, 0);
  negative = max(-value, 0);
  rows = size(field, 1);
  cols = size(field, 2);
  rgb = zeros(rows, cols, 3);
  rgb(:, :, 1) = 1 - 0.85 * negative;
  rgb(:, :, 2) = 1 - 0.85 * positive - 0.85 * negative;
  rgb(:, :, 3) = 1 - 0.85 * positive;
end
%=============================================================================
