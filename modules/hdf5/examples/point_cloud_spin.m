%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Save a 3-D point cloud to HDF5, read it back and spin it in real time.
if ~exist('h5CloudFigureVisible', 'var')
  h5CloudFigureVisible = 'on';
end
if ~exist('h5CloudAnimationEnabled', 'var')
  h5CloudAnimationEnabled = true;
end
if ~exist('h5CloudAnimationFrames', 'var')
  h5CloudAnimationFrames = 720;
end
if ~exist('h5CloudFramePause', 'var')
  h5CloudFramePause = 0;
end
if ~exist('h5CloudAnimationContinuous', 'var')
  h5CloudAnimationContinuous = strcmp(h5CloudFigureVisible, 'on');
end
if ~exist('h5CloudPointCount', 'var')
  h5CloudPointCount = 4000;
end

% Build a trefoil-knot point cloud and store it in an HDF5 file.
parameter = linspace(0, 2 * pi, h5CloudPointCount);
cloudX = sin(parameter) + 2 * sin(2 * parameter);
cloudY = cos(parameter) - 2 * cos(2 * parameter);
cloudZ = -sin(3 * parameter);
cloudColor = parameter;
h5CloudFile = [tempname(), '.h5'];
if isfile(h5CloudFile)
  delete(h5CloudFile);
end
% Group the four coordinate datasets under a single /cloud group.
h5create(h5CloudFile, '/cloud/x', h5CloudPointCount);
h5create(h5CloudFile, '/cloud/y', h5CloudPointCount);
h5create(h5CloudFile, '/cloud/z', h5CloudPointCount);
h5create(h5CloudFile, '/cloud/c', h5CloudPointCount);
h5write(h5CloudFile, '/cloud/x', cloudX);
h5write(h5CloudFile, '/cloud/y', cloudY);
h5write(h5CloudFile, '/cloud/z', cloudZ);
h5write(h5CloudFile, '/cloud/c', cloudColor);
% Read the cloud straight back from HDF5: the file drives the picture.
h5CloudX = h5read(h5CloudFile, '/cloud/x');
h5CloudY = h5read(h5CloudFile, '/cloud/y');
h5CloudZ = h5read(h5CloudFile, '/cloud/z');
h5CloudC = h5read(h5CloudFile, '/cloud/c');

h5CloudFigure = figure('Name', 'HDF5 point cloud', 'NumberTitle', 'off', ...
  'Color', [0.03 0.04 0.07], 'Visible', h5CloudFigureVisible, ...
  'Position', [140 70 800 800]);
h5CloudAxes = axes('Parent', h5CloudFigure, 'Position', ...
  [0.03 0.05 0.94 0.90], 'Color', [0.03 0.04 0.07]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(h5CloudAxes);
h5CloudScatter = scatter3(h5CloudAxes, h5CloudX, h5CloudY, h5CloudZ, 10, ...
  h5CloudC, 'filled');
axis(h5CloudAxes, 'equal');
axis(h5CloudAxes, 'off');
set(h5CloudAxes, 'XLim', [-3.2 3.2], 'YLim', [-3.2 3.2], 'ZLim', [-1.4 1.4]);
title(h5CloudAxes, 'Trefoil cloud read from HDF5', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
h5CloudInfo = uicontrol('Parent', h5CloudFigure, 'Style', 'text', ...
  'String', sprintf('%d points read from the HDF5 group /cloud (x y z c)', ...
  h5CloudPointCount), 'Position', [16 8 700 20], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.03 0.04 0.07], ...
  'ForegroundColor', [0.76 0.84 0.96], 'FontSize', 10);

if h5CloudAnimationEnabled
  h5CloudFrame = 0;
  while isgraphics(h5CloudFigure)
    h5CloudFrame = h5CloudFrame + 1;
    if h5CloudFrame > h5CloudAnimationFrames && ~h5CloudAnimationContinuous
      break
    end
    if ~isgraphics(h5CloudFigure)
      break
    end
    try
      % Spin the camera so the knot turns smoothly in space.
      view(h5CloudAxes, h5CloudFrame * 1.3, 22);
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if h5CloudFramePause > 0
      pause(h5CloudFramePause);
    end
  end
end
%=============================================================================
