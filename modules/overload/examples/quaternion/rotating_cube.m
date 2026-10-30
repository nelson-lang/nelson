%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Spin a 3-D cube by composing rotations with the overloaded quaternion *.
if ~exist('quatCubeFigureVisible', 'var')
  quatCubeFigureVisible = 'on';
end
if ~exist('quatCubeAnimationEnabled', 'var')
  quatCubeAnimationEnabled = true;
end
if ~exist('quatCubeAnimationFrames', 'var')
  quatCubeAnimationFrames = 1400;
end
if ~exist('quatCubeFramePause', 'var')
  quatCubeFramePause = 0;
end
if ~exist('quatCubeAnimationContinuous', 'var')
  quatCubeAnimationContinuous = strcmp(quatCubeFigureVisible, 'on');
end

% Make the @quaternion class folder visible when the demo is run by path.
addpath(fileparts(mfilename('fullpath')));

quatCubeVertices = [-1 -1 -1; 1 -1 -1; 1 1 -1; -1 1 -1; ...
  -1 -1 1; 1 -1 1; 1 1 1; -1 1 1];
quatCubeFaces = [1 2 3 4; 5 6 7 8; 1 2 6 5; 2 3 7 6; 3 4 8 7; 4 1 5 8];
quatCubeFaceColors = [0.95 0.35 0.32; 0.30 0.72 0.46; 0.26 0.52 0.96; ...
  0.98 0.80 0.30; 0.60 0.40 0.85; 0.20 0.78 0.80];
% A fixed tilted spin axis, as a small per-frame rotation quaternion.
quatCubeAxis = [0.40 0.72 0.57];
quatCubeAxis = quatCubeAxis / norm(quatCubeAxis);
quatCubeAngle = 0.035;
quatCubeStep = quaternion(cos(quatCubeAngle / 2), ...
  sin(quatCubeAngle / 2) * quatCubeAxis(1), ...
  sin(quatCubeAngle / 2) * quatCubeAxis(2), ...
  sin(quatCubeAngle / 2) * quatCubeAxis(3));
quatCubeOrientation = quaternion(1, 0, 0, 0);

quatCubeFigure = figure('Name', 'Quaternion cube', 'NumberTitle', 'off', ...
  'Color', [0.05 0.06 0.10], 'Visible', quatCubeFigureVisible, ...
  'Position', [160 90 760 780]);
quatCubeAxes = axes('Parent', quatCubeFigure, ...
  'Position', [0.04 0.08 0.92 0.86], 'Color', [0.05 0.06 0.10]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(quatCubeAxes);
quatCubePatch = patch('Parent', quatCubeAxes, 'Vertices', ...
  quatCubeVertices, 'Faces', quatCubeFaces, 'FaceVertexCData', ...
  quatCubeFaceColors, 'FaceColor', 'flat', 'EdgeColor', [0.1 0.12 0.16], ...
  'LineWidth', 1.4);
view(quatCubeAxes, 3);
axis(quatCubeAxes, 'equal');
axis(quatCubeAxes, 'off');
set(quatCubeAxes, 'XLim', [-2 2], 'YLim', [-2 2], 'ZLim', [-2 2]);
title(quatCubeAxes, 'Cube spun by the overloaded quaternion *', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
quatCubeInfo = uicontrol('Parent', quatCubeFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 720 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.10], 'ForegroundColor', [0.76 0.84 0.96], ...
  'FontSize', 10);

if quatCubeAnimationEnabled
  quatCubeFrame = 0;
  while isgraphics(quatCubeFigure)
    quatCubeFrame = quatCubeFrame + 1;
    if quatCubeFrame > quatCubeAnimationFrames && ...
        ~quatCubeAnimationContinuous
      break
    end
    if ~isgraphics(quatCubeFigure)
      break
    end
    try
      % Compose the running orientation with the step, using the * overload.
      quatCubeOrientation = normalize(quatCubeOrientation * quatCubeStep);
      rotation = rotationMatrix(quatCubeOrientation);
      rotatedVertices = (rotation * quatCubeVertices.').';
      set(quatCubePatch, 'Vertices', rotatedVertices);
      if isgraphics(quatCubeInfo)
        set(quatCubeInfo, 'String', sprintf(...
          'orientation q = %.2f + %.2fi + %.2fj + %.2fk', ...
          quatCubeOrientation.w, quatCubeOrientation.x, ...
          quatCubeOrientation.y, quatCubeOrientation.z));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if quatCubeFramePause > 0
      pause(quatCubeFramePause);
    end
  end
end
%=============================================================================
