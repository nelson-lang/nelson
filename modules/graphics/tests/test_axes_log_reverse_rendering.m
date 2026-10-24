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
f = figure('Visible', 'off', 'Position', [0 0 400 300]);
ax = axes('Parent', f, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', ax, 'XData', [1 10 100], 'YData', [1 10 100], 'Color', [1 0 1]);
set(ax, 'XScale', 'log', 'YScale', 'log', 'XLim', [1 100], 'YLim', [1 100]);
for xdir = {'normal', 'reverse'}
  for ydir = {'normal', 'reverse'}
    set(ax, 'XDir', xdir{1}, 'YDir', ydir{1});
    drawnow();
    frame = __web_display_list__(f);
    % Data, picking, clipping and tick coordinates must share log-space limits.
    point = jsondecode(__web_figure_pick__(f, 200, 150));
    asserts.istrue(point.found);
    asserts.isequal(point.index, 2);
    asserts.isequal(point.position(:)', [10 10 0]);
    asserts.isapprox(point.pixel(:)', [200 150], 1e-6);
    asserts.istrue(contains(frame, 'rgba(255,0,255,1.000000)'));
    asserts.istrue(numel(strfind(frame, '"kind":"text"')) >= 6);
  end
end
close(f);
