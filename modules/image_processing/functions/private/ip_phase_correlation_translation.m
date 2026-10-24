%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [dx, dy, peakcorr] = ip_phase_correlation_translation(moving, fixed)
  moving = moving - mean(moving(:));
  fixed = fixed - mean(fixed(:));
  if all(moving(:) == 0) || all(fixed(:) == 0)
    dx = 0;
    dy = 0;
    peakcorr = 0;
    return
  end
  cps = fft2(fixed) .* conj(fft2(moving));
  cpsAbs = abs(cps);
  keep = cpsAbs > 0;
  cps(keep) = cps(keep) ./ cpsAbs(keep);
  cps(~keep) = 0;
  corr = real(conj(fft2(conj(cps))) / numel(cps));
  [peakcorr, index] = max(corr(:));
  [row, col] = ind2sub(size(corr), index);
  dy = row - 1;
  dx = col - 1;
  rows = size(corr, 1);
  cols = size(corr, 2);
  if dy > floor(rows / 2)
    dy = dy - rows;
  end
  if dx > floor(cols / 2)
    dx = dx - cols;
  end
end
%=============================================================================
