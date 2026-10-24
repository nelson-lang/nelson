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
skip_if_portable_graphics_backend('manual knob tick rendering differs in portable rendering.');
% Non-regression: setting MajorTicks (or MinorTicks/MajorTickLabels) on a
% continuous uiknob must switch the matching *Mode to 'manual', auto labels
% must follow the manual ticks and auto ticks must track Limits. Previously
% 'MajorTicks',[0 50 100] kept MajorTicksMode 'auto' and the stale auto labels
% {'0','20','40',...}, so the knob drew 0, 20, 40 at the 0, 50, 100 positions.
%=============================================================================
fig = uifigure('Visible', 'off');
%=============================================================================
% Manual MajorTicks given at creation: mode manual, labels follow the ticks.
k = uiknob(fig, 'Limits', [0 100], 'MajorTicks', [0 50 100]);
assert_isequal(k.MajorTicks, [0 50 100]);
assert_isequal(k.MajorTicksMode, 'manual');
assert_isequal(k.MajorTickLabelsMode, 'auto');
assert_isequal(k.MajorTickLabels, {'0', '50', '100'});
%=============================================================================
% Manual ticks persist across a Limits change.
k.Limits = [0 200];
assert_isequal(k.MajorTicks, [0 50 100]);
assert_isequal(k.MajorTicksMode, 'manual');
%=============================================================================
% MinorTicks assignment flips its own mode to manual.
k.MinorTicks = 0:10:100;
assert_isequal(k.MinorTicks, 0:10:100);
assert_isequal(k.MinorTicksMode, 'manual');
%=============================================================================
% MajorTickLabels assignment flips its own mode to manual and is kept.
k.MajorTickLabels = {'min', 'mid', 'max'};
assert_isequal(k.MajorTickLabelsMode, 'manual');
k.MajorTicks = [0 100 200];
assert_isequal(k.MajorTickLabels, {'min', 'mid', 'max'});
%=============================================================================
% Untouched knob: auto ticks and labels track Limits.
ka = uiknob(fig);
assert_isequal(ka.MajorTicksMode, 'auto');
assert_isequal(ka.MajorTicks, [0 20 40 60 80 100]);
assert_isequal(ka.MinorTicks, []);
ka.Limits = [0 10];
assert_isequal(ka.MajorTicks, [0 2 4 6 8 10]);
assert_isequal(ka.MajorTickLabels, {'0', '2', '4', '6', '8', '10'});
assert_isequal(ka.MinorTicksMode, 'auto');
delete(fig);
%=============================================================================
% Render check: auto labels following manual ticks must draw exactly like the
% same labels given explicitly (stale labels drew 0, 20, 40 instead).
fig1 = uifigure('Position', [100 100 420 260]);
uiknob(fig1, 'Limits', [0 100], 'MajorTicks', [0 50 100], 'Position', [30 60 80 80]);
drawnow();
F1 = getframe(fig1);
fig2 = uifigure('Position', [100 100 420 260]);
uiknob(fig2, 'Limits', [0 100], 'MajorTicks', [0 50 100], ...
  'MajorTickLabels', {'0', '50', '100'}, 'Position', [30 60 80 80]);
drawnow();
F2 = getframe(fig2);
fig3 = uifigure('Position', [100 100 420 260]);
uiknob(fig3, 'Limits', [0 100], 'MajorTicks', [0 50 100], ...
  'MajorTickLabels', {'0', '20', '40'}, 'Position', [30 60 80 80]);
drawnow();
F3 = getframe(fig3);
assert_isequal(size(F1.cdata), size(F2.cdata));
assert_isequal(F1.cdata, F2.cdata);
% The explicit-vs-auto label difference is only observable when the knob
% actually rendered; a display that cannot paint returns identical blank
% frames, which is an environment limit, not a label regression.
if any(F1.cdata(:) ~= F1.cdata(1))
  assert_isfalse(isequal(F1.cdata, F3.cdata));
end
delete(fig1);
delete(fig2);
delete(fig3);
%=============================================================================
