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
ax1 = nexttile;
asserts.istrue(isgraphics(ax1, 'axes'));
ax2 = nexttile;
asserts.istrue(isgraphics(ax2, 'axes'));
asserts.isfalse(ax1 == ax2);
close all;
%=============================================================================
f = figure('Visible', 'off');
t = tiledlayout(f, 2, 1);
ax1 = nexttile(t);
plot(1:10);
ax2 = nexttile(t);
plot((1:10) .^ 2);
axesHandles = findobj(f, 'Type', 'axes');
asserts.isequal(numel(axesHandles), 2);
asserts.istrue(any(axesHandles == ax1));
asserts.istrue(any(axesHandles == ax2));
close(f);
%=============================================================================
