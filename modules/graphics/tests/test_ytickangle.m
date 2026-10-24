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
ax = gca();
assert_isequal(ax.YTickLabelRotation, 0);
ytickangle(45);
assert_isequal(ax.YTickLabelRotation, 45);
assert_isequal(ax.YTickLabelRotationMode, 'manual');
assert_isequal(ytickangle(), 45);
ytickangle(ax, 90);
assert_isequal(ytickangle(ax), 90);
close(f);
%=============================================================================
close all
%=============================================================================
