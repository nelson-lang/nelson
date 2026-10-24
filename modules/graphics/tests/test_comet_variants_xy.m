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
comet(ax, [1 2], [2 3]);
asserts.isequal(numel(findobj(ax, 'Type', 'animatedline')), 2);
asserts.isequal(numel(findobj(ax, 'Type', 'line')), 1);
close(f);
close all
%=============================================================================
