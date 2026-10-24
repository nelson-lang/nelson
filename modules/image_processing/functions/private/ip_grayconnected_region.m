%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = ip_grayconnected_region(values, row, col, tolerance, conn)
  [rows, cols] = size(values);
  seed = values(row, col);
  allowed = abs(values - seed) <= tolerance;
  bw = false(rows, cols);
  if ~allowed(row, col)
    return
  end
  offsets = ip_connectivity_offsets(conn);
  queue = zeros(numel(values), 1);
  head = 1;
  tail = 1;
  queue(tail) = row + (col - 1) * rows;
  bw(row, col) = true;
  while head <= tail
    idx = queue(head);
    head = head + 1;
    r = mod(idx - 1, rows) + 1;
    c = floor((idx - 1) / rows) + 1;
    for k = 1:size(offsets, 1)
      rr = r + offsets(k, 1);
      cc = c + offsets(k, 2);
      if rr >= 1 && rr <= rows && cc >= 1 && cc <= cols && allowed(rr, cc) && ~bw(rr, cc)
        bw(rr, cc) = true;
        tail = tail + 1;
        queue(tail) = rr + (cc - 1) * rows;
      end
    end
  end
end
%=============================================================================
