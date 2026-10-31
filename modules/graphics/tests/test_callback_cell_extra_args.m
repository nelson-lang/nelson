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
% A cell array callback {@fcn, a, b} calls fcn(src, evt, a, b): exactly two
% arguments before the user data, nothing appended after them.
%=============================================================================
global CELL_CB_ARGS
CELL_CB_ARGS = {};
f = figure('Position', [100 100 400 300]);
ax = axes('Parent', f, 'Position', [0.13 0.11 0.775 0.815]);
plot(ax, 1:10, 'DisplayName', 'linear');
lg = legend(ax);
lg.ItemHitFcn = {@record_cell_args, 42, 'x'};
drawnow();
W = f.Position(3);
H = f.Position(4);
p = lg.Position;
rowH = max(10, ceil(lg.FontSize * 1.35));
y0 = p(2) * H + p(4) * H - 4;
baseX = p(1) * W + 6;
__figure_mouse_event__(f, 'press', [baseX + 3, y0 - rowH * 0.5], 'web');
drawnow();
pause(0.2);
assert_isequal(numel(CELL_CB_ARGS), 1);
assert_isequal(CELL_CB_ARGS{1}, 4);
%=============================================================================
CELL_CB_ARGS = {};
f.SizeChangedFcn = {@record_cell_args, 1, 2, 3};
f.Position = [100 100 500 400];
drawnow();
pause(0.2);
assert_istrue(numel(CELL_CB_ARGS) >= 1);
assert_isequal(CELL_CB_ARGS{1}, 5);
%=============================================================================
close(f);
clear global CELL_CB_ARGS
%=============================================================================
function record_cell_args(varargin)
  global CELL_CB_ARGS
  CELL_CB_ARGS{end + 1} = nargin;
end
%=============================================================================
