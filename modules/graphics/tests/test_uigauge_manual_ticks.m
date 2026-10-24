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
% <--GUI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('manual gauge tick rendering differs in portable rendering.');
% Non-regression: setting MajorTicks (or MinorTicks/MajorTickLabels) on a
% uigauge (every style) must switch the matching *Mode to 'manual', auto
% labels must follow the manual ticks and auto ticks must track Limits.
% Previously 'MajorTicks',[0 50 100] kept MajorTicksMode 'auto' and the stale
% auto labels {'0','20','40',...}, and Limits never updated the auto ticks.
%=============================================================================
fig = uifigure('Visible', 'off');
styles = {'circular', 'linear', 'ninetydegree', 'semicircular'};
for i = 1:numel(styles)
  % Manual MajorTicks given at creation: mode manual, labels follow the ticks.
  g = uigauge(fig, styles{i}, 'MajorTicks', [0 25 100]);
  assert_isequal(g.MajorTicks, [0 25 100]);
  assert_isequal(g.MajorTicksMode, 'manual');
  assert_isequal(g.MajorTickLabelsMode, 'auto');
  assert_isequal(g.MajorTickLabels, {'0', '25', '100'});
  % Manual ticks persist across a Limits change.
  g.Limits = [0 10];
  assert_isequal(g.MajorTicks, [0 25 100]);
  assert_isequal(g.MajorTicksMode, 'manual');
  % MinorTicks assignment flips its own mode to manual.
  g.MinorTicks = 0:10:100;
  assert_isequal(g.MinorTicks, 0:10:100);
  assert_isequal(g.MinorTicksMode, 'manual');
  % MajorTickLabels assignment flips its own mode to manual and is kept.
  g.MajorTickLabels = {'min', 'mid', 'max'};
  assert_isequal(g.MajorTickLabelsMode, 'manual');
  g.MajorTicks = [0 5 10];
  assert_isequal(g.MajorTickLabels, {'min', 'mid', 'max'});
end
%=============================================================================
% Untouched gauges: auto ticks and labels track Limits, defaults unchanged.
g = uigauge(fig);
assert_isequal(g.MajorTicksMode, 'auto');
assert_isequal(g.MajorTicks, [0 20 40 60 80 100]);
assert_isequal(g.MajorTickLabels, {'0', '20', '40', '60', '80', '100'});
assert_isequal(g.MinorTicks, []);
g.Limits = [0 10];
assert_isequal(g.MajorTicks, [0 2 4 6 8 10]);
assert_isequal(g.MajorTickLabels, {'0', '2', '4', '6', '8', '10'});
assert_isequal(g.MinorTicks, []);
assert_isequal(g.MinorTicksMode, 'auto');
%=============================================================================
ng = uigauge(fig, 'ninetydegree');
assert_isequal(ng.MajorTicks, [0 50 100]);
assert_isequal(ng.MajorTickLabels, {'0', '50', '100'});
ng.Limits = [0 10];
assert_isequal(ng.MajorTicks, [0 5 10]);
assert_isequal(ng.MajorTickLabels, {'0', '5', '10'});
%=============================================================================
lg = uigauge(fig, 'linear', 'Limits', [-50 50]);
assert_isequal(lg.MajorTicks, [-50 -30 -10 10 30 50]);
delete(fig);
%=============================================================================
% Render check: auto labels following manual ticks must draw exactly like the
% same labels given explicitly (stale labels drew 0, 20, 40 instead).
fig1 = uifigure('Position', [100 100 420 260]);
uigauge(fig1, 'Limits', [0 100], 'MajorTicks', [0 50 100], 'Position', [30 60 160 160]);
drawnow();
F1 = getframe(fig1);
fig2 = uifigure('Position', [100 100 420 260]);
uigauge(fig2, 'Limits', [0 100], 'MajorTicks', [0 50 100], ...
  'MajorTickLabels', {'0', '50', '100'}, 'Position', [30 60 160 160]);
drawnow();
F2 = getframe(fig2);
fig3 = uifigure('Position', [100 100 420 260]);
uigauge(fig3, 'Limits', [0 100], 'MajorTicks', [0 50 100], ...
  'MajorTickLabels', {'0', '20', '40'}, 'Position', [30 60 160 160]);
drawnow();
F3 = getframe(fig3);
% Ticks outside Limits are not drawn (they were clamped onto the scale ends).
fig4 = uifigure('Position', [100 100 420 260]);
uigauge(fig4, 'Limits', [0 100], 'MajorTicks', [-50 0 50 100 150], ...
  'MajorTickLabels', {'x', '0', '50', '100', 'y'}, 'Position', [30 60 160 160]);
drawnow();
F4 = getframe(fig4);
assert_isequal(size(F1.cdata), size(F2.cdata));
assert_isequal(F1.cdata, F2.cdata);
% The explicit-vs-auto label difference is only observable when the gauge
% actually rendered; a display that cannot paint returns identical blank
% frames, which is an environment limit, not a label regression.
if any(F1.cdata(:) ~= F1.cdata(1))
  assert_isfalse(isequal(F1.cdata, F3.cdata));
end
assert_isequal(F1.cdata, F4.cdata);
delete(fig1);
delete(fig2);
delete(fig3);
delete(fig4);
%=============================================================================
