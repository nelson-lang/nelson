%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_bwmorph_bridge(bw)
  out = bw;
  candidates = find(~bw);
  for k = 1:numel(candidates)
    [r, c] = ind2sub(size(bw), candidates(k));
    r1 = max(1, r - 1); r2 = min(size(bw, 1), r + 1);
    c1 = max(1, c - 1); c2 = min(size(bw, 2), c + 1);
    block = bw(r1:r2, c1:c2);
    block(r - r1 + 1, c - c1 + 1) = false;
    cc = bwconncomp(block, 8);
    if nnz(block) >= 2 && cc.NumObjects >= 2
      out(r, c) = true;
    end
  end
end
%=============================================================================
