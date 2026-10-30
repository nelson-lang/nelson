%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Describe a bouncing-ball scene in XML, read it back and animate it.
if ~exist('xmlSceneFigureVisible', 'var')
  xmlSceneFigureVisible = 'on';
end
if ~exist('xmlSceneAnimationEnabled', 'var')
  xmlSceneAnimationEnabled = true;
end
if ~exist('xmlSceneAnimationFrames', 'var')
  xmlSceneAnimationFrames = 1200;
end
if ~exist('xmlSceneFramePause', 'var')
  xmlSceneFramePause = 0;
end
if ~exist('xmlSceneAnimationContinuous', 'var')
  xmlSceneAnimationContinuous = strcmp(xmlSceneFigureVisible, 'on');
end
if ~exist('xmlSceneBallCount', 'var')
  xmlSceneBallCount = 14;
end
rng(42);

% Build a scene as a struct, write it to XML, then read it straight back:
% the XML file is the single source of truth for the animation.
xmlSceneStruct = struct();
for ballIndex = 1:xmlSceneBallCount
  angle = 2 * pi * rand();
  speed = 0.004 + 0.006 * rand();
  xmlSceneStruct.scene.ball(ballIndex) = struct(...
    'x', 0.1 + 0.8 * rand(), 'y', 0.1 + 0.8 * rand(), ...
    'vx', speed * cos(angle), 'vy', speed * sin(angle), ...
    'r', 0.02 + 0.04 * rand(), ...
    'cr', 0.3 + 0.7 * rand(), 'cg', 0.3 + 0.7 * rand(), ...
    'cb', 0.3 + 0.7 * rand());
end
xmlSceneFile = [tempname(), '.xml'];
writestruct(xmlSceneStruct, xmlSceneFile);
xmlSceneLoaded = readstruct(xmlSceneFile);
xmlSceneBalls = xmlSceneLoaded.scene.ball;
xmlScenePositionX = [xmlSceneBalls.x];
xmlScenePositionY = [xmlSceneBalls.y];
xmlSceneVelocityX = [xmlSceneBalls.vx];
xmlSceneVelocityY = [xmlSceneBalls.vy];
xmlSceneRadius = [xmlSceneBalls.r];
xmlSceneColors = [[xmlSceneBalls.cr].' [xmlSceneBalls.cg].' ...
  [xmlSceneBalls.cb].'];

xmlSceneFigure = figure('Name', 'XML scene', 'NumberTitle', 'off', ...
  'Color', [0.04 0.05 0.09], 'Visible', xmlSceneFigureVisible, ...
  'Position', [140 80 760 780]);
xmlSceneAxes = axes('Parent', xmlSceneFigure, 'Position', ...
  [0.04 0.06 0.92 0.88], 'Color', [0.04 0.05 0.09]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(xmlSceneAxes);
hold(xmlSceneAxes, 'on');
xmlSceneMarkers = gobjects(1, xmlSceneBallCount);
for ballIndex = 1:xmlSceneBallCount
  xmlSceneMarkers(ballIndex) = plot(xmlSceneAxes, ...
    xmlScenePositionX(ballIndex), xmlScenePositionY(ballIndex), 'o', ...
    'MarkerFaceColor', xmlSceneColors(ballIndex, :), ...
    'MarkerEdgeColor', 'none', 'LineStyle', 'none', ...
    'MarkerSize', 6 + 220 * xmlSceneRadius(ballIndex));
end
hold(xmlSceneAxes, 'off');
axis(xmlSceneAxes, 'equal');
axis(xmlSceneAxes, 'off');
set(xmlSceneAxes, 'XLim', [0 1], 'YLim', [0 1]);
title(xmlSceneAxes, 'Bouncing balls described entirely in XML', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
xmlSceneInfo = uicontrol('Parent', xmlSceneFigure, 'Style', 'text', ...
  'String', sprintf('%d balls read with readstruct from an XML scene', ...
  xmlSceneBallCount), 'Position', [16 8 700 20], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.04 0.05 0.09], ...
  'ForegroundColor', [0.76 0.84 0.96], 'FontSize', 10);

if xmlSceneAnimationEnabled
  xmlSceneFrame = 0;
  while isgraphics(xmlSceneFigure)
    xmlSceneFrame = xmlSceneFrame + 1;
    if xmlSceneFrame > xmlSceneAnimationFrames && ...
        ~xmlSceneAnimationContinuous
      break
    end
    if ~isgraphics(xmlSceneFigure)
      break
    end
    try
      xmlScenePositionX = xmlScenePositionX + xmlSceneVelocityX;
      xmlScenePositionY = xmlScenePositionY + xmlSceneVelocityY;
      % Reflect off the four walls, accounting for each ball radius.
      lowX = xmlScenePositionX < xmlSceneRadius;
      highX = xmlScenePositionX > 1 - xmlSceneRadius;
      xmlSceneVelocityX(lowX | highX) = -xmlSceneVelocityX(lowX | highX);
      lowY = xmlScenePositionY < xmlSceneRadius;
      highY = xmlScenePositionY > 1 - xmlSceneRadius;
      xmlSceneVelocityY(lowY | highY) = -xmlSceneVelocityY(lowY | highY);
      xmlScenePositionX = min(max(xmlScenePositionX, xmlSceneRadius), ...
        1 - xmlSceneRadius);
      xmlScenePositionY = min(max(xmlScenePositionY, xmlSceneRadius), ...
        1 - xmlSceneRadius);
      for ballIndex = 1:xmlSceneBallCount
        set(xmlSceneMarkers(ballIndex), 'XData', ...
          xmlScenePositionX(ballIndex), 'YData', ...
          xmlScenePositionY(ballIndex));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if xmlSceneFramePause > 0
      pause(xmlSceneFramePause);
    end
  end
end
%=============================================================================
