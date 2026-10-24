%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% spy() reads a marker out of its LineSpec. That marker has to reach the drawn
% object: parsed and then dropped, every spy() plot comes out wearing the
% default marker of whatever primitive spy happens to draw with.
%=============================================================================
% The dot is the default, and it is the marker the plot must show.
f = figure('Visible', 'off');
spy(eye(3));
assert_isequal(gca().Children(1).Marker, '.');
close(f);
%=============================================================================
% Every marker a LineSpec can carry reaches the drawn object.
lineSpecMarkers = {'+', 'o', '*', '.', 'x', 's', 'd', '^', 'v', '>', '<', 'p', 'h'};
drawnMarkers = {'+', 'o', '*', '.', 'x', 'square', 'diamond', '^', 'v', '>', '<', ...
  'pentagram', 'hexagram'};
for k = 1:numel(lineSpecMarkers)
  f = figure('Visible', 'off');
  spy(eye(3), ['r', lineSpecMarkers{k}]);
  assert_isequal(gca().Children(1).Marker, drawnMarkers{k}, ...
    ['LineSpec r', lineSpecMarkers{k}, ' must draw ', drawnMarkers{k}, '.']);
  close(f);
end
%=============================================================================
% An empty matrix has no entry to mark.
emptyMatrices = {[], sparse([])};
for k = 1:numel(emptyMatrices)
  f = figure('Visible', 'off');
  spy(emptyMatrices{k});
  assert_isequal(gca().Children(1).Marker, 'none');
  close(f);
end
%=============================================================================
% An all-zero matrix is not an empty one: it keeps its marker, it simply has
% no location to put one at.
f = figure('Visible', 'off');
spy(zeros(3));
assert_isequal(gca().Children(1).Marker, '.');
close(f);
%=============================================================================
% A marker size is a LENGTH, and scatter() sizes its markers by AREA. The
% requested length has to be squared on the way in, otherwise every spy()
% marker is drawn at the square root of the size that was asked for.
f = figure('Visible', 'off');
spy(eye(3), 'r*', 12);
assert_isequal(gca().Children(1).SizeData, 144);
close(f);
%=============================================================================
f = figure('Visible', 'off');
spy(eye(3), 12);
assert_isequal(gca().Children(1).SizeData, 144);
close(f);
%=============================================================================
% 6 points is the default size of every marker that is not the dot.
f = figure('Visible', 'off');
spy(eye(3), 'r*');
assert_isequal(gca().Children(1).SizeData, 36);
close(f);
%=============================================================================
% The marker has to be painted, not merely recorded. '.', '*', '+' and 'x' are
% stroked shapes with no face of their own: asking for a filled marker hands
% their edge to 'none' and they come out blank.
f = figure('Visible', 'off', 'Position', [100 100 500 400]);
spy(eye(20), 'r');
drawnow();
painted = count_red_pixels(getframe(f).cdata);
close(f);
assert_istrue(painted > 100, 'spy() must paint its markers.');
%=============================================================================
% And it has to be painted at the size that was asked for: a marker size is a
% length, so trebling it trebles the width of the mark instead of multiplying
% it by sqrt(3). The check is a ratio, so it does not depend on the display.
narrowWidth = spy_marker_width(10);
wideWidth = spy_marker_width(30);
assert_istrue(narrowWidth > 0, 'spy() must paint its markers.');
assert_istrue(wideWidth / narrowWidth > 2.5, 'spy() marker size must be a length.');
assert_istrue(wideWidth / narrowWidth < 3.5, 'spy() marker size must be a length.');
%=============================================================================
function n = count_red_pixels(img)
  mask = red_mask(img);
  n = sum(mask(:));
end
%=============================================================================
function w = spy_marker_width(markersize)
  f = figure('Visible', 'off', 'Position', [100 100 500 400]);
  spy(1, 'rs', markersize);
  drawnow();
  columns = find(any(red_mask(getframe(f).cdata), 1));
  close(f);
  if isempty(columns)
    w = 0;
  else
    w = max(columns) - min(columns) + 1;
  end
end
%=============================================================================
function mask = red_mask(img)
  mask = (double(img(:, :, 1)) > 180) & (double(img(:, :, 2)) < 100) & ...
    (double(img(:, :, 3)) < 100);
end
%=============================================================================
