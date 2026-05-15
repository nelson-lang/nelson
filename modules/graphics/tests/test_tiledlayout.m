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
t = tiledlayout(2, 2);
assert_istrue(isgraphics(t, 'tiledlayout'));
assert_isequal(t.GridSize, [2, 2]);
assert_isequal(t.TileArrangement, 'fixed');
assert_isequal(t.TileSpacing, 'loose');
assert_isequal(t.Padding, 'loose');
close all;
%=============================================================================
t2 = tiledlayout('flow');
assert_istrue(isgraphics(t2, 'tiledlayout'));
assert_isequal(t2.TileArrangement, 'flow');
close all;
%=============================================================================
t2b = tiledlayout();
assert_istrue(isgraphics(t2b, 'tiledlayout'));
assert_isequal(t2b.TileArrangement, 'flow');
close all;
%=============================================================================
t2c = tiledlayout('flow', 'TileSpacing', 'compact', 'Padding', 'none');
assert_isequal(t2c.TileArrangement, 'flow');
assert_isequal(t2c.TileSpacing, 'compact');
assert_isequal(t2c.Padding, 'tight');
close all;
%=============================================================================
t3 = tiledlayout(3, 4, 'TileSpacing', 'compact', 'Padding', 'tight');
assert_isequal(t3.GridSize, [3, 4]);
assert_isequal(t3.TileSpacing, 'compact');
assert_isequal(t3.Padding, 'tight');
close all;
%=============================================================================
t4 = tiledlayout('TileSpacing', 'normal');
assert_isequal(t4.TileSpacing, 'loose');
t4.GridSize = [3 1];
assert_isequal(t4.GridSize, [3 1]);
assert_isequal(t4.TileArrangement, 'fixed');
assert_checkerror('t4.TileArrangement = ''flow'';', _('Property is readable only: TileArrangement'));
close all;
%=============================================================================
assert_checkerror('tiledlayout(2)', _('tiledlayout(m,n) requires both m and n grid dimensions.'));
assert_checkerror('tiledlayout(2, 0)', _('n must be a positive integer scalar.'));
assert_checkerror('tiledlayout(''diagonal'')', _('Name-value arguments must appear in pairs.'));
assert_checkerror('tiledlayout(''Bogus'', 1)', _('Unknown property: Bogus'));
%=============================================================================
t5 = tiledlayout(2, 2);
[tt, ss] = title(t5, 'Shared Title', 'Shared Subtitle');
xx = xlabel(t5, 'Shared X');
yy = ylabel(t5, 'Shared Y');
assert_istrue(isgraphics(tt, 'text'));
assert_istrue(isgraphics(ss, 'text'));
assert_istrue(isgraphics(xx, 'text'));
assert_istrue(isgraphics(yy, 'text'));
assert_isequal(t5.Title.String, 'Shared Title');
assert_isequal(t5.Subtitle.String, 'Shared Subtitle');
assert_isequal(t5.XLabel.String, 'Shared X');
assert_isequal(t5.YLabel.String, 'Shared Y');
close all;
%=============================================================================
t6 = tiledlayout();
assert_checkerror('set(t6, ''Bogus'', 1)', _('Unknown property: Bogus'));
close all;
%=============================================================================
