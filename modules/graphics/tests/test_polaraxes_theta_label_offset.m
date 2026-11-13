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
% Theta labels sit a fixed pixel gap outside the circle, whatever the axes size.
deg = char(176);
for side = [110 260 560]
  [f, pax] = newPolarAxes(side, 10);
  assert_istrue(all(abs(labelGapPixels(pax) - expectedGap(10)) < 1e-6));
  close(f);
end
%=============================================================================
% The gap grows with the axes font, which the labels follow.
[f, pax] = newPolarAxes(260, 20);
assert_istrue(all(abs(labelGapPixels(pax) - expectedGap(20)) < 1e-6));
assert_istrue(all(labelFontSizes(pax) == 20));
pax.FontSize = 14;
paint(f);
assert_istrue(all(labelFontSizes(pax) == 14));
assert_istrue(all(abs(labelGapPixels(pax) - expectedGap(14)) < 1e-6));
close(f);
%=============================================================================
% A resize keeps the gap in pixels.
[f, pax] = newPolarAxes(260, 10);
pax.Position = [120 120 120 120];
paint(f);
assert_istrue(all(abs(labelGapPixels(pax) - expectedGap(10)) < 1e-6));
close(f);
%=============================================================================
% Labels are aligned on the side facing the circle.
[f, pax] = newPolarAxes(260, 10);
assertAlignment(pax, ['0', deg], 'left', 'middle');
assertAlignment(pax, ['30', deg], 'left', 'bottom');
assertAlignment(pax, ['90', deg], 'center', 'bottom');
assertAlignment(pax, ['150', deg], 'right', 'bottom');
assertAlignment(pax, ['180', deg], 'right', 'middle');
assertAlignment(pax, ['270', deg], 'center', 'top');
assertAlignment(pax, ['330', deg], 'left', 'top');
pax.ThetaZeroLocation = 'top';
pax.ThetaDir = 'clockwise';
paint(f);
assertAlignment(pax, ['0', deg], 'center', 'bottom');
assertAlignment(pax, ['90', deg], 'left', 'middle');
assertAlignment(pax, ['270', deg], 'right', 'middle');
assert_istrue(all(abs(labelGapPixels(pax) - expectedGap(10)) < 1e-6));
close(f);
%=============================================================================
% Wedges keep the same gap.
for lim = {[0 90], [0 180]}
  [f, pax] = newPolarAxes(260, 10);
  pax.ThetaLim = lim{1};
  paint(f);
  assert_istrue(all(abs(labelGapPixels(pax) - expectedGap(10)) < 1e-6));
  close(f);
end
%=============================================================================
% The title bottom sits about 22.7 px above the circle top (reference), 2.2 px without labels.
[f, pax] = newPolarAxes(260, 10);
t = title(pax, 'T');
paint(f);
assert_istrue(abs(titleGapPixels(pax, t) - 22.73) < 1.5);
pax.ThetaTickLabel = {};
paint(f);
assert_istrue(abs(titleGapPixels(pax, t) - 2.2) < 1e-6);
close(f);
[f, pax] = newPolarAxes(110, 20);
t = title(pax, 'T');
paint(f);
assert_istrue(abs(titleGapPixels(pax, t) - 42.0) < 1.5);
close(f);
%=============================================================================
function [f, pax] = newPolarAxes(side, fontSize)
  f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [50 50 side + 240 side + 240]);
  pax = polaraxes(f, 'Units', 'pixels', 'Position', [120 120 side side]);
  pax.FontSize = fontSize;
  paint(f);
end
%=============================================================================
function paint(f)
  % A hidden figure lays out its polar labels when it is rendered.
  drawnow();
  getframe(f);
end
%=============================================================================
function gap = expectedGap(fontSize)
  gap = 3.3 + 0.24 * fontSize * 96 / 72;
end
%=============================================================================
function scale = pixelsPerUnit(pax)
  position = pax.Position;
  scale = min(position(3) / diff(pax.XLim), position(4) / diff(pax.YLim));
end
%=============================================================================
function labels = thetaLabels(pax)
  labels = findall(pax, 'Type', 'text', 'Tag', '__nelson_polar_grid__');
  rmax = max(abs(pax.RLim));
  keep = false(size(labels));
  for k = 1:numel(labels)
    p = labels(k).Position;
    keep(k) = hypot(p(1), p(2)) > rmax;
  end
  labels = labels(keep);
  assert_istrue(~isempty(labels));
end
%=============================================================================
function gaps = labelGapPixels(pax)
  labels = thetaLabels(pax);
  rmax = max(abs(pax.RLim));
  gaps = zeros(numel(labels), 1);
  for k = 1:numel(labels)
    p = labels(k).Position;
    gaps(k) = (hypot(p(1), p(2)) - rmax) * pixelsPerUnit(pax);
  end
end
%=============================================================================
function sizes = labelFontSizes(pax)
  labels = findall(pax, 'Type', 'text', 'Tag', '__nelson_polar_grid__');
  sizes = arrayfun(@(h) h.FontSize, labels);
end
%=============================================================================
function assertAlignment(pax, str, horizontal, vertical)
  t = findall(pax, 'Type', 'text', 'Tag', '__nelson_polar_grid__', 'String', str);
  assert_isequal(numel(t), 1);
  assert_isequal(char(t.HorizontalAlignment), horizontal);
  assert_isequal(char(t.VerticalAlignment), vertical);
end
%=============================================================================
function gap = titleGapPixels(pax, t)
  gap = (t.Position(2) - pax.YLim(2)) * pixelsPerUnit(pax);
end
%=============================================================================
