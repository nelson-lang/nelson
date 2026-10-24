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
skip_if_portable_graphics_backend('knob font color rendering differs in portable rendering.');
% Non-regression: the knob ticks and tick labels are drawn in FontColor.
% Previously they were always near-black, invisible on a dark background.
%=============================================================================
function n = yellowPixels(fontColor)
  f = uifigure('Position', [100 100 300 200], 'Color', [0.05 0.05 0.08]);
  uiknob(f, 'Limits', [0 100], 'MajorTicks', [0 50 100], ...
    'MinorTicks', 0:10:100, 'FontColor', fontColor, 'Position', [100 50 80 80]);
  drawnow();
  F = getframe(f);
  close(f);
  C = double(F.cdata);
  n = nnz(C(:, :, 1) > 200 & C(:, :, 2) > 200 & C(:, :, 3) < 60);
end
%=============================================================================
assert_istrue(yellowPixels([1 1 0]) > 50);
assert_isequal(yellowPixels([0.13 0.13 0.13]), 0);
%=============================================================================
% FontColor changed after creation is applied too.
f = uifigure('Position', [100 100 300 200], 'Color', [0.05 0.05 0.08]);
k = uiknob(f, 'MajorTicks', [0 50 100], 'Position', [100 50 80 80]);
drawnow();
k.FontColor = [1 1 0];
drawnow();
F = getframe(f);
close(f);
C = double(F.cdata);
assert_istrue(nnz(C(:, :, 1) > 200 & C(:, :, 2) > 200 & C(:, :, 3) < 60) > 50);
%=============================================================================
% discrete knob: item labels follow FontColor as well
f = uifigure('Position', [100 100 300 250], 'Color', [0.05 0.05 0.08]);
uiknob(f, 'discrete', 'FontColor', [1 1 0], 'Position', [100 80 60 60]);
drawnow();
F = getframe(f);
close(f);
C = double(F.cdata);
assert_istrue(nnz(C(:, :, 1) > 200 & C(:, :, 2) > 200 & C(:, :, 3) < 60) > 50);
%=============================================================================
