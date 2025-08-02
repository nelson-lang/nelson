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
    -1 -1  1;
    -1  1 -1;
    -1  1  1;
     1 -1 -1;
     1 -1  1;
     1  1 -1;
     1  1  1
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

% Generate random lines inside the cube
numLines = 1000; % Increased number of lines
randLines = -1 + 2 * rand(numLines, 6); % Random points in range [-1,1]
colors = rand(numLines, 3); % Generate random colors

% Create figure
f = figure('Visible', 'off');
axis equal;
axis off
grid on;
view(3);
hold on;

% Plot cube edges
for i = 1:size(edges, 1)
    plot3(vertices(edges(i, :), 1), vertices(edges(i, :), 2), vertices(edges(i, :), 3), 'k', 'LineWidth', 1);
end

% Plot random lines inside the cube with multiple colors
plot3([randLines(:,1), randLines(:,4)]', ...
      [randLines(:,2), randLines(:,5)]', ...
      [randLines(:,3), randLines(:,6)]', 'LineWidth', 0.5);
f.Visible = 'on';

% Rotate the cube
for angle = 1:3600
  if (isgraphics(f))
    view(angle, 30);
  end
end
