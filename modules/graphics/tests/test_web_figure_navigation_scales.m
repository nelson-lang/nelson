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
f = figure('Visible', 'off');
ax = axes('Parent', f); plot(ax, [1 10 100], [1 10 100]);
fh = __web_graphics_handle__(f, 'number');
ah = __web_graphics_handle__(ax, 'number');
for scale = {'linear', 'log'}
  for direction = {'normal', 'reverse'}
    set(ax, 'XScale', scale{1}, 'YScale', scale{1}, ...
      'XDir', direction{1}, 'YDir', direction{1}, 'XLim', [1 100], 'YLim', [1 100]);
    drawnow();
    __web_figure_action__(fh, jsonencode(struct('action', 'reference', 'axesHandle', ah)));
    __web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, 'dxFrac', 0.25, 'dyFrac', 0.25)));
    sign = 1;
    if strcmp(direction{1}, 'reverse')
      sign = -1;
    end
    if strcmp(scale{1}, 'log')
      asserts.isapprox(log10(ax.XLim), [0 2] - sign * 0.5, 1e-8);
      asserts.isapprox(log10(ax.YLim), [0 2] + sign * 0.5, 1e-8);
    else
      asserts.isapprox(ax.XLim, [1 100] - sign * 24.75, 1e-8);
      asserts.isapprox(ax.YLim, [1 100] + sign * 24.75, 1e-8);
    end
    __web_figure_action__(fh, jsonencode(struct('action', 'restore', 'axesHandle', ah)));
    __web_figure_action__(fh, jsonencode(struct('action', 'zoom', 'axesHandle', ah, 'factor', 2)));
    if strcmp(scale{1}, 'log')
      asserts.isapprox(log10(ax.XLim), [0.5 1.5], 1e-8);
      asserts.isapprox(log10(ax.YLim), [0.5 1.5], 1e-8);
    else
      asserts.isapprox(ax.XLim, [25.75 75.25], 1e-8);
      asserts.isapprox(ax.YLim, [25.75 75.25], 1e-8);
    end
    __web_figure_action__(fh, jsonencode(struct('action', 'zoom', 'axesHandle', ah, 'factor', 0.5)));
    asserts.isapprox(ax.XLim, [1 100], 1e-8);
    asserts.isapprox(ax.YLim, [1 100], 1e-8);
  end
end
caught = false;
try
  __web_figure_action__(fh, jsonencode(struct('action', 'zoom', 'axesHandle', ah, 'factor', 0)));
catch
  caught = true;
end
asserts.istrue(caught);
close(f);
