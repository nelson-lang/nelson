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
figureObject = figure('Visible', 'off');
a1 = subplot(1, 2, 1); plot([1 10 100]);
a2 = subplot(1, 2, 2); surf(peaks(8));
drawnow();
fh = __web_graphics_handle__(figureObject, 'number');
ah = __web_graphics_handle__(a1, 'number');
other = [a2.XLim a2.YLim a2.ZLim a2.View];
set(a1, 'XScale', 'log', 'XDir', 'reverse', 'XLim', [1 100], 'YLim', [0 10]);
request = struct('action', 'reference', 'axesHandle', ah);
reply = jsondecode(__web_figure_action__(fh, jsonencode(request), false));
asserts.istrue(isfield(reply, 'state'));
asserts.isfalse(isfield(reply, 'displayList'));
request.action = 'pan'; request.dxFrac = 0.25; request.dyFrac = 0.1;
__web_figure_action__(fh, jsonencode(request), false);
asserts.isapprox(log10(a1.XLim), [0.5 2.5], 1e-10);
asserts.isequal([a2.XLim a2.YLim a2.ZLim a2.View], other);
request.action = 'restore';
__web_figure_action__(fh, jsonencode(request), false);
asserts.isapprox(a1.XLim, [1 100], 1e-10);
request.action = 'zoom'; request.factor = 2;
__web_figure_action__(fh, jsonencode(request), false);
asserts.isapprox(log10(a1.XLim), [0.5 1.5], 1e-10);
request.action = 'select';
__web_figure_action__(fh, jsonencode(request), false);
asserts.isequal(figureObject.CurrentAxes, a1);
request.action = 'fit';
__web_figure_action__(fh, jsonencode(request), false);
asserts.isequal(a1.XLimMode, 'auto');
close(figureObject);
