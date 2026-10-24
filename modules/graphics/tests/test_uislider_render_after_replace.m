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
skip_if_portable_graphics_backend('slider getframe colors differ in portable rendering.');
% Non-regression: a slider whose Position is set AFTER creation (as a generic
% layout helper does) must still render its track, thumb and tick labels.
% Previously the drawable area was only expanded on the first placement, so a
% re-laid-out slider collapsed to its 3px track height and painted nothing.
%=============================================================================
function n = yellowPixels(replace)
  f = uifigure('Position', [100 100 360 160], 'Color', [0.05 0.05 0.08]);
  if replace
    s = uislider(f, 'Limits', [0 100], 'Value', 40, 'FontColor', [1 1 0], ...
      'Position', [10 10 200 3]);
    s.Position = [30 80 300 3];
  else
    s = uislider(f, 'Limits', [0 100], 'Value', 40, 'FontColor', [1 1 0], ...
      'Position', [30 80 300 3]);
  end
  drawnow();
  F = getframe(f);
  close(f);
  C = double(F.cdata);
  n = nnz(C(:, :, 1) > 200 & C(:, :, 2) > 200 & C(:, :, 3) < 60);
end
%=============================================================================
% A slider placed once renders its yellow tick labels (sanity check).
assert_istrue(yellowPixels(false) > 50);
% A slider re-placed after creation must render just the same.
assert_istrue(yellowPixels(true) > 50);
%=============================================================================
