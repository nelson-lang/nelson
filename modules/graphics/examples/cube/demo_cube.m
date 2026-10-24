%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Define cube vertices
vertices = [
    -1 -1 -1;
    -1 -1 1;
    -1 1 -1;
    -1 1 1;
     1 -1 -1;
     1 -1 1;
     1 1 -1;
     1 1 1
];

% Define cube edges
edges = [
    1 2; 1 3; 1 5;
    2 4; 2 6;
    3 4; 3 7;
    4 8;
    5 6; 5 7;
    6 8;
    7 8
];

numLines = 1000;
randLines = -1 + 2 * rand(numLines, 6);
colorCount = 64;
colors = rand(colorCount, 3);
lineGroups = mod(0:numLines - 1, colorCount) + 1;

% Create figure
f = figure('Visible', 'off');
ax = axes('Parent', f);
axis off
grid on;
view(3);
hold on;

edgeCount = size(edges, 1);
edgeX = [vertices(edges(:, 1), 1), vertices(edges(:, 2), 1), nan(edgeCount, 1)]';
edgeY = [vertices(edges(:, 1), 2), vertices(edges(:, 2), 2), nan(edgeCount, 1)]';
edgeZ = [vertices(edges(:, 1), 3), vertices(edges(:, 2), 3), nan(edgeCount, 1)]';
plot3(edgeX(:), edgeY(:), edgeZ(:), 'k', 'LineWidth', 1);

for colorIndex = 1:colorCount
  selected = lineGroups == colorIndex;
  groupCount = sum(selected);
  lineX = [randLines(selected, 1), randLines(selected, 4), nan(groupCount, 1)]';
  lineY = [randLines(selected, 2), randLines(selected, 5), nan(groupCount, 1)]';
  lineZ = [randLines(selected, 3), randLines(selected, 6), nan(groupCount, 1)]';
  plot3(lineX(:), lineY(:), lineZ(:), 'Color', colors(colorIndex, :), 'LineWidth', 0.5);
end

% Equal data units with fixed symmetric limits that contain the whole cube.
% Setting DataAspectRatio (instead of calling 'axis equal' on the empty axes,
% which would freeze the limits before any data exists) keeps the cube fully
% inside the axes box.
set(ax, 'DataAspectRatio', [1, 1, 1]);
xlim(ax, [-1, 1]);
ylim(ax, [-1, 1]);
zlim(ax, [-1, 1]);

f.Visible = 'on';
drawnow();

framesPerSecond = 60;
duration = 10;
% allow tests (or users) to shorten the animation
durationOverride = str2double(getenv('NELSON_DEMO_DURATION'));
if isfinite(durationOverride) && durationOverride > 0
  duration = durationOverride;
end
frameCount = framesPerSecond * duration;
frameDelay = 1 / framesPerSecond;
timerStart = tic();
for k = 1:frameCount
  if ~isgraphics(f)
    break
  end
  angle = 360 * (k - 1) / framesPerSecond;
  view(ax, angle, 30);
  drawnow();
  elapsedTime = toc(timerStart);
  pause(max(0, frameDelay - elapsedTime));
  timerStart = tic();
end
