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
xticks([0 2 4 6 8 10]);
ax = gca();
assert_isequal(ax.XTick, [0 2 4 6 8 10]);
assert_isequal(ax.XTickMode, 'manual');
assert_isequal(xticks(), [0 2 4 6 8 10]);
assert_isequal(xticks('mode'), 'manual');
xticks('auto');
assert_isequal(ax.XTickMode, 'auto');
xticks('manual');
assert_isequal(ax.XTickMode, 'manual');
xticks(ax, [1 5 10]);
assert_isequal(ax.XTick, [1 5 10]);
xticks((0:5)');
assert_isequal(ax.XTick, 0:5);
close(f);
%=============================================================================
assert_checkerror('xticks(1, 2, 3)', getString(message('Nelson:narginchk:tooManyInputs')));
%=============================================================================
close all
%=============================================================================
