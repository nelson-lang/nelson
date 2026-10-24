%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Floyd-Steinberg error-diffusion dithering. I is an M-by-N-by-3 image in
% [0, 1]; map is K-by-3. Returns an M-by-N array of zero-based indices.
function idx = ip_dither_assign(I, map)
  [rows, cols, ~] = size(I);
  r = I(:, :, 1);
  g = I(:, :, 2);
  b = I(:, :, 3);
  idx = zeros(rows, cols);
  mr = map(:, 1);
  mg = map(:, 2);
  mb = map(:, 3);
  for row = 1:rows
    for col = 1:cols
      or = r(row, col);
      og = g(row, col);
      ob = b(row, col);
      d = (mr - or) .^ 2 + (mg - og) .^ 2 + (mb - ob) .^ 2;
      [~, k] = min(d);
      idx(row, col) = k - 1;
      er = or - mr(k);
      eg = og - mg(k);
      eb = ob - mb(k);
      if col < cols
        r(row, col + 1) = r(row, col + 1) + er * 7 / 16;
        g(row, col + 1) = g(row, col + 1) + eg * 7 / 16;
        b(row, col + 1) = b(row, col + 1) + eb * 7 / 16;
      end
      if row < rows
        if col > 1
          r(row + 1, col - 1) = r(row + 1, col - 1) + er * 3 / 16;
          g(row + 1, col - 1) = g(row + 1, col - 1) + eg * 3 / 16;
          b(row + 1, col - 1) = b(row + 1, col - 1) + eb * 3 / 16;
        end
        r(row + 1, col) = r(row + 1, col) + er * 5 / 16;
        g(row + 1, col) = g(row + 1, col) + eg * 5 / 16;
        b(row + 1, col) = b(row + 1, col) + eb * 5 / 16;
        if col < cols
          r(row + 1, col + 1) = r(row + 1, col + 1) + er * 1 / 16;
          g(row + 1, col + 1) = g(row + 1, col + 1) + eg * 1 / 16;
          b(row + 1, col + 1) = b(row + 1, col + 1) + eb * 1 / 16;
        end
      end
    end
  end
end
%=============================================================================
