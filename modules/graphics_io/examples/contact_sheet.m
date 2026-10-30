%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Render plasma frames, write them with imwrite and read them into a sheet.
if ~exist('gioFigureVisible', 'var')
  gioFigureVisible = 'on';
end
if ~exist('gioAnimationEnabled', 'var')
  gioAnimationEnabled = true;
end
if ~exist('gioFramePause', 'var')
  gioFramePause = 0.08;
end
if ~exist('gioAnimationContinuous', 'var')
  gioAnimationContinuous = strcmp(gioFigureVisible, 'on');
end
if ~exist('gioGrid', 'var')
  gioGrid = 4;
end
if ~exist('gioThumbSize', 'var')
  gioThumbSize = 150;
end

gioCount = gioGrid * gioGrid;
gioSheetSize = gioGrid * gioThumbSize;
gioSheet = zeros(gioSheetSize, gioSheetSize, 3, 'uint8');
gioDirectory = tempname();
mkdir(gioDirectory);

gioFigure = figure('Name', 'imwrite contact sheet', 'NumberTitle', 'off', ...
  'Color', [0.04 0.04 0.06], 'Visible', gioFigureVisible, ...
  'Position', [140 70 780 820]);
gioAxes = axes('Parent', gioFigure, 'Position', [0.03 0.08 0.94 0.86]);
% Prevent accidental pan or zoom from disturbing the live contact sheet.
disableDefaultInteractivity(gioAxes);
gioImage = image(gioAxes, 'CData', gioSheet);
axis(gioAxes, 'image');
axis(gioAxes, 'off');
title(gioAxes, 'Plasma frames written and read back with imwrite / imread', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 12);
gioInfo = uicontrol('Parent', gioFigure, 'Style', 'text', 'String', '', ...
  'Position', [16 8 700 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.04 0.04 0.06], 'ForegroundColor', [0.76 0.84 0.96], ...
  'FontSize', 10);

if gioAnimationEnabled
  gioFrame = 0;
  gioPhaseBase = 0;
  while isgraphics(gioFigure)
    gioFrame = gioFrame + 1;
    slot = mod(gioFrame - 1, gioCount) + 1;
    if slot == 1
      gioPhaseBase = gioPhaseBase + 1.1;
    end
    if ~gioAnimationContinuous && gioFrame > gioCount
      break
    end
    if ~isgraphics(gioFigure)
      break
    end
    try
      % Render one plasma thumbnail, save it, then read it straight back.
      thumbnail = gioPlasma(gioThumbSize, gioPhaseBase + slot * 0.6);
      thumbFile = [gioDirectory, filesep(), sprintf('frame_%02d.png', slot)];
      imwrite(thumbnail, thumbFile);
      loaded = imread(thumbFile);
      rowBlock = floor((slot - 1) / gioGrid);
      colBlock = mod(slot - 1, gioGrid);
      rows = rowBlock * gioThumbSize + (1:gioThumbSize);
      cols = colBlock * gioThumbSize + (1:gioThumbSize);
      gioSheet(rows, cols, :) = loaded;
      set(gioImage, 'CData', gioSheet);
      if isgraphics(gioInfo)
        set(gioInfo, 'String', sprintf(...
          'thumbnail %d of %d written with imwrite and read with imread', ...
          slot, gioCount));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if gioFramePause > 0
      pause(gioFramePause);
    end
  end
end
%=============================================================================
function image = gioPlasma(side, phase)
  % A colourful plasma pattern returned as an 8-bit RGB image.
  axis1d = linspace(0, 4 * pi, side);
  [x, y] = meshgrid(axis1d, axis1d);
  field = sin(x + phase) + sin(y + phase * 0.7) + ...
    sin((x + y) / 2 + phase * 1.3) + sin(sqrt(x .^ 2 + y .^ 2) + phase);
  red = 0.5 + 0.5 * sin(field);
  green = 0.5 + 0.5 * sin(field + 2.1);
  blue = 0.5 + 0.5 * sin(field + 4.2);
  image = uint8(255 * cat(3, red, green, blue));
end
%=============================================================================
