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
f = figure('Visible', 'off');
plot(sin(0:0.1:2 * pi));
ax = gca();
zoom('reset');
initialXLim = ax.XLim;
initialYLim = ax.YLim;
zoom(1.5);
asserts.istrue(diff(ax.XLim) < diff(initialXLim));
asserts.istrue(diff(ax.YLim) < diff(initialYLim));
zoom('out');
asserts.isequal(ax.XLim, initialXLim);
asserts.isequal(ax.YLim, initialYLim);
zoom(ax, 'off');
zoom(f, 'on');
zoom(f, 'xon');
zoom(f, 'yon');
zoom(f, 'off');
zoom();
close(f);
%=============================================================================
% a figure or axes handle on its own toggles zoom instead of raising
% 'Index exceeds array bounds'
f = figure('Visible', 'off');
plot(1:10);
ax = gca();
zoom(f);
zoom(ax);
close(f);
%=============================================================================
