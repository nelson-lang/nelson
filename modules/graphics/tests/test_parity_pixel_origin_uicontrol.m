%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Parity with the reference: the first pixel of a parent is at 1 for the ui components too, so
% the pixel Position has x and y shifted by 1 against the other units (which start at 0).
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
drawnow();
scale = [600 400 600 400];
%=============================================================================
c = uicontrol(f, 'Style', 'pushbutton', 'Units', 'pixels', 'Position', [20 20 60 30]);
drawnow();
assert_istrue(close_to(c.Position, [20 20 60 30]));
c.Units = 'normalized';
assert_istrue(close_to(c.Position .* scale, [19 19 60 30]));
c.Units = 'pixels';
assert_istrue(close_to(c.Position, [20 20 60 30]));
c.Units = 'points';
assert_istrue(close_to(c.Position, [14.25 14.25 45 22.5]));
c.Units = 'inches';
assert_istrue(close_to(c.Position, [19 19 60 30] / 96));
c.Units = 'normalized';
c.Position = [0.1 0.1 0.2 0.2];
c.Units = 'pixels';
assert_istrue(close_to(c.Position, [61 41 120 80]));
%=============================================================================
% the default position of a uicontrol is [20 20 60 20]
c2 = uicontrol(f, 'Style', 'pushbutton');
assert_istrue(close_to(c2.Position, [20 20 60 20]));
%=============================================================================
c4 = uicontrol(f, 'Style', 'text', 'Units', 'normalized', 'Position', [0.25 0.5 0.1 0.1]);
c4.Units = 'pixels';
assert_istrue(close_to(c4.Position, [151 201 60 40]));
%=============================================================================
p = uipanel(f, 'Units', 'pixels', 'Position', [20 20 300 200], 'BorderType', 'none');
drawnow();
assert_istrue(close_to(p.Position, [20 20 300 200]));
p.Units = 'normalized';
assert_istrue(close_to(p.Position .* scale, [19 19 300 200]));
p.Units = 'pixels';
assert_istrue(close_to(p.Position, [20 20 300 200]));
% the origin of a child is the first pixel of the panel
cc = uicontrol(p, 'Style', 'pushbutton', 'Units', 'pixels', 'Position', [10 10 50 20]);
drawnow();
cc.Units = 'normalized';
assert_istrue(close_to(cc.Position .* [300 200 300 200], [9 9 50 20]));
cc.Units = 'pixels';
assert_istrue(close_to(cc.Position, [10 10 50 20]));
%=============================================================================
t = uitable(f, 'Units', 'pixels', 'Position', [5 5 100 100]);
drawnow();
t.Units = 'normalized';
assert_istrue(close_to(t.Position .* scale, [4 4 100 100]));
close(f);
%=============================================================================
function ok = close_to(actual, expected)
  ok = isequal(size(actual), size(expected)) && all(abs(actual - expected) < 1e-6);
end
%=============================================================================
