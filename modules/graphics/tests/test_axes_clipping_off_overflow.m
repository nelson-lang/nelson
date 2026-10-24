%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
% Axes 'Clipping' property controls the PIXEL clip to the axes Position
% rectangle, not just the data-limit clip. With Clipping == 'off', foreground
% geometry that overflows the axes box must spill into the figure instead of
% being cut at the axes-rectangle edge.
%
% The axes here fills only the right part of the figure, and a red patch
% extends well to the left of the axes left limit (x < XLim(1)), so that part
% maps to pixels LEFT of the axes rectangle. With Clipping == 'on' nothing
% appears in that left strip; with Clipping == 'off' the patch spills into it.
%=============================================================================
figPos = [100 100 500 400];
axesLeftFraction = 0.55;
% Left strip: everything well to the left of the axes rectangle.
stripRight = floor(axesLeftFraction * figPos(3)) - 25;

coverageOn = left_strip_red_fraction('on', figPos, axesLeftFraction, stripRight);
coverageOff = left_strip_red_fraction('off', figPos, axesLeftFraction, stripRight);

% Clipping 'on' leaves the strip outside the axes rectangle empty.
asserts.istrue(coverageOn < 0.005);
% Clipping 'off' lets the foreground patch spill into that strip.
asserts.istrue(coverageOff > 0.03);
asserts.istrue(coverageOff > coverageOn + 0.02);
%=============================================================================
function frac = left_strip_red_fraction(clipping, figPos, axesLeftFraction, stripRight)
  f = figure('Visible', 'on', 'Position', figPos, 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Position', [axesLeftFraction 0.12 0.40 0.80]);
  hold(ax, 'on');
  axis(ax, 'off');
  set(ax, 'Color', [1 1 1], 'Clipping', clipping);
  % Patch extends far to the left of XLim(1) so it overflows the axes rect.
  fill(ax, [-3 1 1 -3], [0.2 0.2 0.8 0.8], [1 0 0], 'EdgeColor', 'none');
  set(ax, 'XLim', [0 1], 'YLim', [0 1]);
  drawnow();
  cdata = getframe(f).cdata;
  close(f);
  frac = red_fraction_left(cdata, stripRight);
end
%=============================================================================
function frac = red_fraction_left(cdata, stripRight)
  img = double(cdata);
  cols = min(stripRight, size(img, 2));
  strip = img(:, 1:cols, :);
  red = (strip(:, :, 1) > 150) & (strip(:, :, 2) < 100) & (strip(:, :, 3) < 100);
  frac = sum(red(:)) / (size(strip, 1) * size(strip, 2));
end
%=============================================================================
