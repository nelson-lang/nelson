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
% uilamp has no BackgroundColor: only its disc is painted, the corners of its
% box must show the parent colour (figure Color, uipanel BackgroundColor),
% not a light square.
%=============================================================================
bg = [0.03 0.04 0.07];
f = uifigure('Visible', 'off', 'Position', [100 100 300 100], 'Color', bg);
uilamp(f, 'Position', [20 40 20 20], 'Color', [0 1 0]);
p = uipanel(f, 'Position', [150 10 140 80], 'BackgroundColor', [0.1 0.1 0.3], ...
  'BorderType', 'none');
uilamp(p, 'Position', [10 10 20 20], 'Color', [1 0 0]);
drawnow();
F = getframe(f);
close(f);
C = double(F.cdata);
H = size(C, 1);
sx = size(C, 2) / 300;
sy = H / 100;
% pixel at figure coordinates (x, y), y measured from the bottom
pix = @(x, y) squeeze(C(H - round(y * sy), 1 + round(x * sx), :))';
%=============================================================================
% lamp on the figure: corner = figure Color, centre = lamp Color
assert_istrue(max(abs(pix(21, 58) - 255 * bg)) <= 8);
assert_istrue(max(abs(pix(39, 41) - 255 * bg)) <= 8);
assert_istrue(max(abs(pix(30, 50) - [0 255 0])) <= 8);
%=============================================================================
% lamp on a uipanel: corner = panel BackgroundColor
assert_istrue(max(abs(pix(161, 38) - 255 * [0.1 0.1 0.3])) <= 8);
assert_istrue(max(abs(pix(170, 30) - [255 0 0])) <= 8);
%=============================================================================
