%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
t = tiledlayout(2, 2);
asserts.istrue(isgraphics(t, 'tiledlayout'));
asserts.isequal(t.GridSize, [2, 2]);
asserts.isequal(t.TileArrangement, 'fixed');
asserts.isequal(t.TileSpacing, 'loose');
asserts.isequal(t.Padding, 'loose');
close all;
%=============================================================================
t2 = tiledlayout('flow');
asserts.istrue(isgraphics(t2, 'tiledlayout'));
asserts.isequal(t2.TileArrangement, 'flow');
close all;
%=============================================================================
t2b = tiledlayout();
asserts.istrue(isgraphics(t2b, 'tiledlayout'));
asserts.isequal(t2b.TileArrangement, 'flow');
close all;
%=============================================================================
t2c = tiledlayout('flow', 'TileSpacing', 'compact', 'Padding', 'none');
asserts.isequal(t2c.TileArrangement, 'flow');
asserts.isequal(t2c.TileSpacing, 'compact');
asserts.isequal(t2c.Padding, 'tight');
close all;
%=============================================================================
t3 = tiledlayout(3, 4, 'TileSpacing', 'compact', 'Padding', 'tight');
asserts.isequal(t3.GridSize, [3, 4]);
asserts.isequal(t3.TileSpacing, 'compact');
asserts.isequal(t3.Padding, 'tight');
close all;
%=============================================================================
t4 = tiledlayout('TileSpacing', 'normal');
asserts.isequal(t4.TileSpacing, 'loose');
t4.GridSize = [3 1];
asserts.isequal(t4.GridSize, [3 1]);
asserts.isequal(t4.TileArrangement, 'fixed');
asserts.checkerror('t4.TileArrangement = ''flow'';', getString(message('Nelson:graphics:readOnlyProperty', 'TileArrangement')));
close all;
%=============================================================================
asserts.checkerror('tiledlayout(2)', getString(message('Nelson:tiledlayout:missingGridDimensions')));
asserts.checkerror('tiledlayout(2, 0)', getString(message('Nelson:graphics:invalidInteger', 'n')));
asserts.checkerror('tiledlayout(''diagonal'')', getString(message('Nelson:graphics:nameValuePair')));
asserts.checkerror('tiledlayout(''Bogus'', 1)', getString(message('Nelson:tiledlayout:unknownProperty', 'Bogus')));
%=============================================================================
t5 = tiledlayout(2, 2);
[tt, ss] = title(t5, 'Shared Title', 'Shared Subtitle');
xx = xlabel(t5, 'Shared X');
yy = ylabel(t5, 'Shared Y');
asserts.istrue(isgraphics(tt));
asserts.istrue(isgraphics(ss));
asserts.istrue(isgraphics(xx));
asserts.istrue(isgraphics(yy));
asserts.isfalse(isprop(tt, 'Type'));
asserts.isfalse(isprop(ss, 'Position'));
asserts.isequal(t5.Title.String, 'Shared Title');
asserts.isequal(t5.Subtitle.String, 'Shared Subtitle');
asserts.isequal(t5.XLabel.String, 'Shared X');
asserts.isequal(t5.YLabel.String, 'Shared Y');
close all;
%=============================================================================
t6 = tiledlayout();
asserts.checkerror('set(t6, ''Bogus'', 1)', getString(message('Nelson:graphics:unknownProperty', 'Bogus')));
close all;
%=============================================================================
