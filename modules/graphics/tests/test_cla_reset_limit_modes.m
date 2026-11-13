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
% 'cla reset' and reset() bring limit modes back to auto with default limits.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:10);
set(ax, 'XLim', [0 20], 'YLim', [0 10]);
cla(ax);
assert_isequal(ax.XLimMode, 'manual');
assert_isequal(ax.YLimMode, 'manual');
assert_isequal(ax.YLim, [0 10]);
%=============================================================================
plot(ax, 1:10);
set(ax, 'XLim', [0 20], 'YLim', [0 10], 'XTick', [0 10 20]);
cla(ax, 'reset');
assert_isequal(ax.XLimMode, 'auto');
assert_isequal(ax.YLimMode, 'auto');
assert_isequal(ax.ZLimMode, 'auto');
assert_isequal(ax.XTickMode, 'auto');
assert_isequal(ax.XLim, [0 1]);
assert_isequal(ax.YLim, [0 1]);
assert_isequal(ax.ZLim, [0 1]);
%=============================================================================
line(ax, [0 100], [0 50]);
drawnow();
assert_isequal(ax.XLim, [0 100]);
assert_isequal(ax.YLim, [0 50]);
%=============================================================================
set(ax, 'XLim', [5 6], 'YLim', [1 2]);
reset(ax);
assert_isequal(ax.XLimMode, 'auto');
assert_isequal(ax.YLimMode, 'auto');
close(f);
%=============================================================================
