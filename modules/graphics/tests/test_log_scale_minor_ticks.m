%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
[x, y] = meshgrid(1:4, 1:4);
z = 10 .^ (x + y);
surf(ax, x, y, z);
view(ax, 3);
ax.XScale = 'log';
ax.YScale = 'log';
ax.ZScale = 'log';
drawnow();
asserts.isequal(char(ax.XMinorTick), 'on');
asserts.isequal(char(ax.YMinorTick), 'on');
asserts.isequal(char(ax.ZMinorTick), 'on');
ax.XMinorTick = 'off';
ax.YMinorTick = 'off';
ax.ZMinorTick = 'off';
drawnow();
asserts.isequal(char(ax.XMinorTick), 'off');
asserts.isequal(char(ax.YMinorTick), 'off');
asserts.isequal(char(ax.ZMinorTick), 'off');
close(f);
%=============================================================================
