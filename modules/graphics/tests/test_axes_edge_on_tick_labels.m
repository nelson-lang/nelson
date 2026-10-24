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
% An edge-on 3D view such as view(90, 0) rotates the Z axis onto the screen
% vertical and the Y axis onto the screen horizontal, while the X axis points
% straight into the screen. The vertical-axis tick labels (and the horizontal
% ruler labels) must still be drawn on the correct visible box edge:
%   - The automatic Z range must keep its symmetric nice range ([-15 15]); the
%     forward-facing step-2 re-snap must be skipped for edge-on views.
%   - The projected segment toward the opposite parallel box edge collapses to a
%     point for an edge-on view; the tick-label offset must fall back to the
%     axis' own screen-perpendicular so no label is suppressed.
%=============================================================================
close all
reset(groot());
%=============================================================================
[X, Y] = meshgrid(-6:.5:6);
Z = Y .* sin(X) - X .* cos(Y);
%=============================================================================
% Edge-on Z range must stay the symmetric nice range, not a coarse step-2 grid.
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
ax = axes('Parent', f);
surf(ax, X, Y, Z);
view(ax, 90, 0);
drawnow();
asserts.isequal(ax.ZLim, [-15 15]);
zlabels = cellstr(ax.ZTickLabel);
asserts.istrue(any(strcmp(zlabels, '-15')));
asserts.istrue(any(strcmp(zlabels, '15')));
close(f);
%=============================================================================
% Web backend: the Z tick labels must be emitted in the left gutter (screen
% vertical ruler) and the Y tick labels in the bottom gutter (horizontal ruler).
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
ax = axes('Parent', f);
surf(ax, X, Y, Z);
view(ax, 90, 0);
drawnow();
[labelText, labelX, labelY] = web_text_primitives(f);
close(f);
zTickStrings = {'-15', '-10', '-5', '0', '5', '10', '15'};
for k = 1:numel(zTickStrings)
  asserts.istrue(any(strcmp(labelText, zTickStrings{k})));
end
% Left gutter (vertical Z ruler): several labels at small x.
leftGutter = labelX < 70;
asserts.istrue(sum(leftGutter) >= 5);
% Bottom gutter (horizontal Y ruler): several labels at large y.
bottomGutter = labelY > 380;
asserts.istrue(sum(bottomGutter) >= 5);
%=============================================================================
% Qt backend (getframe): the left label region must contain drawn text pixels.
% getframe returns a device-resolution image (Position x DevicePixelRatio), so
% derive every threshold from the ACTUAL returned image size as fractions rather
% than hardcoded pixel values; this keeps the check DPI-independent.
f = figure('Visible', 'on', 'Position', [100 100 560 420], 'Color', [1 1 1]);
ax = axes('Parent', f);
surf(ax, X, Y, Z);
view(ax, 90, 0);
drawnow();
pause(0.1);
drawnow();
axesPosition = ax.Position;
cdata = getframe(f).cdata;
% Guard against an empty/blank frame if the window has not settled yet: re-draw
% and retry once before giving up on this sub-check.
if isempty(cdata) || numel(unique(cdata(:))) <= 1
  drawnow();
  cdata = getframe(f).cdata;
end
close(f);
asserts.istrue(~isempty(cdata) && numel(unique(cdata(:))) > 1);
imgHeight = size(cdata, 1);
imgWidth = size(cdata, 2);
% Inspect the full left margin, stopping before the axis line. A fixed 11%
% strip can cut through the glyphs at some device scales and font metrics.
gutterWidth = max(1, floor(axesPosition(1) * imgWidth) - 2);
gutter = double(cdata(:, 1:gutterWidth, :));
dark = (gutter(:, :, 1) < 120) & (gutter(:, :, 2) < 120) & (gutter(:, :, 3) < 120);
asserts.istrue(sum(dark(:)) > 20);
% The labels span the vertical ruler top to bottom, not a single cluster:
% a dark label in the upper third and another in the lower third.
darkRows = find(any(dark, 2));
asserts.istrue(min(darkRows) < imgHeight / 3);
asserts.istrue(max(darkRows) > 2 * imgHeight / 3);
%=============================================================================
function [labelText, labelX, labelY] = web_text_primitives(f)
  displayList = jsondecode(__web_display_list__(f));
  primitives = displayList.primitives;
  if ~iscell(primitives)
    primitives = num2cell(primitives);
  end
  labelText = {};
  labelX = [];
  labelY = [];
  for k = 1:numel(primitives)
    p = primitives{k};
    if isfield(p, 'kind') && strcmp(p.kind, 'text')
      labelText{end + 1} = p.text;
      labelX(end + 1) = p.x;
      labelY(end + 1) = p.y;
    end
  end
end
%=============================================================================
