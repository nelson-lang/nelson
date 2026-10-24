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
assert_isequal(ax.XTickLabelRotation, 0);
xtickangle(45);
assert_isequal(ax.XTickLabelRotation, 45);
assert_isequal(ax.XTickLabelRotationMode, 'manual');
assert_isequal(xtickangle(), 45);
xtickangle(-30);
assert_isequal(ax.XTickLabelRotation, -30);
xtickangle(ax, 90);
assert_isequal(xtickangle(ax), 90);
close(f);
%=============================================================================
assert_checkerror('f = figure(''Visible'', ''off''); xtickangle(''foo''); close(f);', getString(message('Nelson:graphics:mustBeNumericScalar', 'Input')));
%=============================================================================
close all
%=============================================================================
