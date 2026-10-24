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
f1 = figure();
p = pie([3, 2, 1], [0, 0, 1]);
asserts.isequal(size(p), [1 6]);
ax = gca();
asserts.isequal(ax.XLim, [-1.2 1.2]);
asserts.isequal(ax.YLim, [-1.2 1.2]);
asserts.isequal(ax.ZLim, [-1 1]);
asserts.isequal(ax.ZTick, [-1 0 1]);
asserts.isapprox(p(1).EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
asserts.isapprox(p(2).Color, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
asserts.isequal([p(1).XData(end), p(1).YData(end)], [0 0]);
asserts.isapprox(p(2).Position, [-1.1 0 0], 1e-12);
close all
%=============================================================================
