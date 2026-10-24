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
% <--GUI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('checkbox / radio indicator getframe differs in portable rendering.');
%=============================================================================
% A classic uicontrol check box or radio button must always draw its indicator
% box or circle. Applying a stylesheet (any FontColor / BackgroundColor) made Qt
% render the indicator through the style-sheet engine and drop the native box,
% so a checked check box showed a bare check mark with no box and a selected
% radio button showed no circle at all. getframe on a visible figure composites
% the real Qt widgets, so it captures that rendering. The indicator box and
% circle are drawn with a gray outline (channels nearly equal, mid brightness);
% the check mark and the radio dot alone do not form that gray outline, so the
% regression (no box / no circle on the selected control) drops this count to
% zero. The controls carry no text, to keep the count to the indicator itself.
%=============================================================================
grayOutlinePixels = @(img) sum(sum( ...
  any(img < 205, 3) & all(img > 70, 3) & ...
  abs(double(img(:, :, 1)) - double(img(:, :, 2))) < 18 & ...
  abs(double(img(:, :, 2)) - double(img(:, :, 3))) < 18 & ...
  abs(double(img(:, :, 1)) - double(img(:, :, 3))) < 18));
%=============================================================================
% A checked check box: the box outline must be present (the regression removed
% the box and kept only the check mark). Measured: 87 with the box, 0 without.
fCheck = figure('Position', [200 200 120 50], 'Color', [1 1 1]);
uicontrol(fCheck, 'Style', 'checkbox', 'String', '', 'Value', 1, ...
  'Position', [8 14 30 24], 'BackgroundColor', [1 1 1]);
drawnow();
checkImage = getframe(fCheck).cdata;
close(fCheck);
assert_istrue(grayOutlinePixels(checkImage) > 30);
%=============================================================================
% A selected radio button: the circle outline must be present (the regression
% removed the circle). Measured: 59 with the circle, 0 without.
fRadio = figure('Position', [200 200 120 50], 'Color', [1 1 1]);
uicontrol(fRadio, 'Style', 'radiobutton', 'String', '', 'Value', 1, ...
  'Position', [8 14 30 24], 'BackgroundColor', [1 1 1]);
drawnow();
radioImage = getframe(fRadio).cdata;
close(fRadio);
assert_istrue(grayOutlinePixels(radioImage) > 20);
%=============================================================================
