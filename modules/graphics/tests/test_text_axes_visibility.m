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
figure();
t = text(0.5, 0.5, 'text here');
s = t.FontSize;
t.FontSize = 12;
drawnow();
ax = gca();
asserts.isequal(s, 10);
asserts.isequal(t.FontSize, 12);
asserts.isequal(ax.XLim, [0 1]);
asserts.isequal(ax.YLim, [0 1]);
asserts.istrue(all(ax.XTick >= 0 & ax.XTick <= 1));
asserts.istrue(all(ax.YTick >= 0 & ax.YTick <= 1));
%=============================================================================
f = figure();
axis off;
t = text(0.5, 0.5, 'visible with hidden axes');
asserts.isequal(char(t.Visible), 'on');
t = text(0.5, 0.4, 'explicitly hidden', 'Visible', 'off');
asserts.isequal(char(t.Visible), 'off');
close(f);
close all;
%=============================================================================
