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
plot3(1:10, 1:10, (1:10) .^ 2);
zticks([0 25 50 75 100]);
ax = gca();
assert_isequal(ax.ZTick, [0 25 50 75 100]);
assert_isequal(ax.ZTickMode, 'manual');
assert_isequal(zticks(), [0 25 50 75 100]);
assert_isequal(zticks('mode'), 'manual');
zticks('auto');
assert_isequal(ax.ZTickMode, 'auto');
zticks('manual');
assert_isequal(ax.ZTickMode, 'manual');
zticks(ax, [0 50 100]);
assert_isequal(ax.ZTick, [0 50 100]);
close(f);
%=============================================================================
close all
%=============================================================================
