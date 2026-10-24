%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
% In 'auto' mode the center label grows to fill the donut hole, so its font
% size scales with the inner radius (bigger hole -> bigger label).
f = figure('Visible', 'off', 'Units', 'pixels', 'InnerPosition', [100 100 420 320], ...
  'Color', [1 1 1]);
d = donutchart([4 3 2], ["A", "B", "C"], 'CenterLabel', '9');
asserts.isequal(d.CenterLabelFontSizeMode, 'auto');
d.InnerRadius = 0.3;
drawnow();
getframe(f);
smallHoleSize = d.CenterLabelFontSize;
d.InnerRadius = 0.7;
drawnow();
getframe(f);
largeHoleSize = d.CenterLabelFontSize;
asserts.istrue(largeHoleSize > smallHoleSize);
asserts.istrue(smallHoleSize > 10);
asserts.isequal(d.CenterLabelFontSizeMode, 'auto');
close(f);
%=============================================================================
% A manual CenterLabelFontSize is honored and never rescaled.
f = figure('Visible', 'off', 'Units', 'pixels', 'InnerPosition', [100 100 420 320], ...
  'Color', [1 1 1]);
d = donutchart([1 1], 'CenterLabel', 'X');
d.CenterLabelFontSize = 22;
drawnow();
getframe(f);
asserts.isequal(d.CenterLabelFontSizeMode, 'manual');
asserts.isequal(d.CenterLabelFontSize, 22);
close(f);
close all
%=============================================================================
