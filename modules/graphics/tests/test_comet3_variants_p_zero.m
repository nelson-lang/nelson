%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
comet3(ax, 1:4, [1 4 2 3], [0 1 0 1], 0);
animated = findobj(ax, 'Type', 'animatedline');
asserts.isequal(graphics_comet3_count_points(animated), [1 4]);
close(f);
close all
%=============================================================================
