%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [f, ax, rendered] = create_animatedline_render_axes()
  f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Position', [0.18 0.18 0.7 0.7]);
  set(ax, 'XLim', [0 1], 'YLim', [0 1]);
  rendered = animatedline(ax, [0.1 0.9], [0.2 0.8], 'Color', [1 0 0], 'LineWidth', 4);
end
