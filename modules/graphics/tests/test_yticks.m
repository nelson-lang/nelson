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
close all
%=============================================================================
f = figure('Visible', 'off');
plot(1:10, (1:10) .^ 2);
yticks([0 25 50 75 100]);
ax = gca();
assert_isequal(ax.YTick, [0 25 50 75 100]);
assert_isequal(ax.YTickMode, 'manual');
assert_isequal(yticks(), [0 25 50 75 100]);
assert_isequal(yticks('mode'), 'manual');
yticks('auto');
assert_isequal(ax.YTickMode, 'auto');
yticks('manual');
assert_isequal(ax.YTickMode, 'manual');
yticks(ax, [0 50 100]);
assert_isequal(ax.YTick, [0 50 100]);
close(f);
%=============================================================================
close all
%=============================================================================
