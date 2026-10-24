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
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
%=============================================================================
y = [3 1 4 1 5];
comet(ax, y);
animated = findobj(ax, 'Type', 'animatedline');
asserts.isequal(numel(animated), 2);
asserts.istrue(graphics_comet_any_full_trace(animated, 1:numel(y), y));
close(f);
close all
%=============================================================================
