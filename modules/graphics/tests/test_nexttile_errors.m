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
t = tiledlayout(1, 1);
nexttile;
asserts.checkerror('nexttile', getString(message('Nelson:nexttile:noEmptyTile')));
close all;
%=============================================================================
f = figure();
t1 = tiledlayout(1, 2);
nexttile(t1);
t2 = tiledlayout('flow');
ax2 = nexttile(t2);
% A second tiledlayout replaces the first one in the same figure.
asserts.isequal(numel(findobj(f, 'Type', 'tiledlayout')), 1);
asserts.istrue(isgraphics(t2, 'tiledlayout'));
asserts.isequal(tilenum(ax2), 1);
close all;
%=============================================================================
