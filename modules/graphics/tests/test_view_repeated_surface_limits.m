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
close all
f = figure('Visible', 'off', 'Position', [100 100 620 460]);
[x, y, z] = peaks(24);
surf(x, y, z);
view(35, 28);
view(45, 30);
drawnow();
ax = gca();
asserts.isequal(ax.View, [45 30]);
asserts.isequal(ax.XLim, [-4 4]);
asserts.isequal(ax.YLim, [-4 4]);
asserts.isequal(ax.ZLim, [-8 8]);
asserts.isequal(ax.ZTick, -8:2:8);
frame = getframe(f).cdata;
addpath([modulepath('graphics', 'tests')]);
render_assert_not_blank(frame);
close all
%=============================================================================
