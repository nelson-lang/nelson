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
ax = gca();
assert_isequal(ax.ZTickLabelRotation, 0);
ztickangle(45);
assert_isequal(ax.ZTickLabelRotation, 45);
assert_isequal(ax.ZTickLabelRotationMode, 'manual');
assert_isequal(ztickangle(), 45);
ztickangle(ax, 90);
assert_isequal(ztickangle(ax), 90);
close(f);
%=============================================================================
close all
%=============================================================================
