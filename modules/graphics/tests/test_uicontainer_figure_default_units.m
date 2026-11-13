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
% containers in a figure fill their parent in normalized units
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
p = uipanel(f);
assert_isequal(p.Units, 'normalized');
assert_isequal(p.Position, [0 0 1 1]);
assert_isequal(p.OuterPosition, [0 0 1 1]);
assert_istrue(all(p.InnerPosition(3:4) > 0.99 & p.InnerPosition(3:4) < 1));
bg = uibuttongroup(f);
assert_isequal(bg.Units, 'normalized');
assert_isequal(bg.Position, [0 0 1 1]);
tg = uitabgroup(f);
assert_isequal(tg.Units, 'normalized');
assert_isequal(tg.Position, [0 0 1 1]);
t = uitab(tg);
assert_isequal(t.Units, 'normalized');
assert_isequal(t.Position, [0 0 1 1]);
nested = uipanel(p);
assert_isequal(nested.Units, 'normalized');
assert_isequal(nested.Position, [0 0 1 1]);
close(f);
%=============================================================================
% a normalized Position is a fraction of the parent
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
p = uipanel(f, 'Position', [.1 .1 .8 .8]);
assert_isequal(p.Units, 'normalized');
assert_isapprox(p.Position, [.1 .1 .8 .8], 1e-12);
p.Units = 'pixels';
assert_isapprox(p.Position, [61 41 480 320], 1e-9);
close(f);
%=============================================================================
% switching to pixels gives the parent size
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
p = uipanel(f);
p.Units = 'pixels';
assert_isequal(p.Position, [1 1 600 400]);
p2 = uipanel(f, 'Units', 'pixels', 'Position', [10 20 200 100]);
assert_isequal(p2.Units, 'pixels');
assert_isequal(p2.Position, [10 20 200 100]);
c = uicontrol(p2);
assert_isequal(c.Units, 'pixels');
close(f);
%=============================================================================
% a tab is as large as its tab group, tab bar included
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
tg = uitabgroup(f);
t = uitab(tg);
c = uicontrol(t, 'Units', 'normalized', 'Position', [0 0 1 1]);
c.Units = 'pixels';
assert_isequal(c.Position, [1 1 800 600]);
t.Units = 'pixels';
assert_isequal(t.Position, [1 1 800 600]);
close(f);
%=============================================================================
% uifigure keeps pixel defaults
uf = uifigure('Visible', 'off');
p = uipanel(uf);
assert_isequal(p.Units, 'pixels');
assert_isequal(p.Position, [20 20 260 221]);
bg = uibuttongroup(uf);
assert_isequal(bg.Units, 'pixels');
assert_isequal(bg.Position, [20 20 260 210]);
tg = uitabgroup(uf);
assert_isequal(tg.Units, 'pixels');
assert_isequal(tg.Position, [20 20 250 210]);
nested = uipanel(p);
assert_isequal(nested.Units, 'pixels');
assert_isequal(nested.Position, [20 20 260 221]);
close(uf);
%=============================================================================
