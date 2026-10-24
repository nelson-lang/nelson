%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A contour label is drawn along the line it names, so its angle is the tangent
% of that line. The direction a line is walked in belongs to the surface and not
% to the reader: the walk keeps the side above the level on its left, so a good
% half of the lines of a plot run right to left, with a tangent near 180
% degrees. Drawn at that angle the label comes out upside down. Half a turn puts
% the text back the right way up without moving it off its line, so the tangent
% is folded into [-90, 90]. The two quarter turns are kept as they are: a
% vertical label reads bottom to top or top to bottom, both readable.
%
% Every angle below was measured, none was derived. Nelson
% folds labels the same way and leaves a quarter turn alone.
%
% The angle a label was drawn at is read back from an SVG export, which writes
% it as the rotate() of the text element (and leaves the transform out when the
% label is level).
%
% Which way each line is walked is read from the contour matrix rather than
% spelled out here: that walk is the subject of test_contour_matrix_vertices,
% and what is asserted here is only the angle the label takes from it.
%=============================================================================
% A surface varying with y alone is cut into one horizontal line, walked right
% to left when the surface is higher below it and left to right when it is
% higher above it. A surface varying with x alone is cut into one vertical line.
grids = {repmat(linspace(10, 0, 5)', 1, 5), repmat(linspace(0, 10, 5)', 1, 5), ...
    repmat(linspace(0, 10, 5), 5, 1), repmat(linspace(10, 0, 5), 5, 1)};
for k = 1:numel(grids)
  contourMatrix = contourc(grids{k}, [4.5 4.5]);
  % one level, one line: a single header followed by its vertices
  asserts.isequal(size(contourMatrix, 2), contourMatrix(2, 1) + 1);
  vertices = contourMatrix(:, 2:end);
  if (vertices(2, 1) == vertices(2, end))
    % A horizontal line reads level whichever way it is walked.
    expectedRotation = 0;
  elseif (vertices(2, end) > vertices(2, 1))
    % Walked upward, the label takes the quarter turn that reads bottom to top.
    expectedRotation = 90;
  else
    expectedRotation = -90;
  end
  svgPath = [tempname(), '.svg'];
  f = figure('Visible', 'off', 'Position', [100 100 560 470]);
  contour(grids{k}, [4.5 4.5], 'ShowText', 'on');
  saveas(f, svgPath);
  close(f);
  svgText = fileread(svgPath);
  rmfile(svgPath);
  rotations = [];
  svgLines = strsplit(svgText, char(10));
  for i = 1:numel(svgLines)
    svgLine = svgLines{i};
    % A label and an axis tick can both read 4.5, so the two are told apart by
    % colour: a contour label is drawn in the LabelColor grey rgba(32,32,32),
    % an axis tick in the slightly lighter ruler grey rgba(33,33,33).
    if ~contains(svgLine, '>4.5</text>') || ~contains(svgLine, 'rgba(32,32,32')
      continue;
    end
    at = strfind(svgLine, 'rotate(');
    if isempty(at)
      rotations(end + 1) = 0;
    else
      rest = svgLine(at(1) + 7:end);
      stop = strfind(rest, ')');
      rotations(end + 1) = -str2double(rest(1:stop(1) - 1));
    end
  end
  asserts.isequal(numel(rotations), 2);
  asserts.isequal(rotations, [expectedRotation, expectedRotation]);
end
%=============================================================================
% The whole reported case, and the two other members of the family that label
% their lines the same way: no label is turned past a quarter turn either way.
%=============================================================================
drawers = {@contour, @contourf, @contour3};
expectedLabels = [17, 17, 4];
for k = 1:numel(drawers)
  svgPath = [tempname(), '.svg'];
  f = figure('Visible', 'off', 'Position', [100 100 560 470]);
  drawers{k}(peaks(20), 'ShowText', 'on');
  saveas(f, svgPath);
  close(f);
  svgText = fileread(svgPath);
  rmfile(svgPath);
  rotations = [];
  svgLines = strsplit(svgText, char(10));
  for i = 1:numel(svgLines)
    svgLine = svgLines{i};
    at = strfind(svgLine, 'rotate(');
    if ~contains(svgLine, '<text ') || isempty(at)
      continue;
    end
    rest = svgLine(at(1) + 7:end);
    stop = strfind(rest, ')');
    rotations(end + 1) = -str2double(rest(1:stop(1) - 1));
  end
  asserts.isequal(numel(rotations), expectedLabels(k));
  asserts.istrue(all(abs(rotations) <= 90));
end
%=============================================================================
