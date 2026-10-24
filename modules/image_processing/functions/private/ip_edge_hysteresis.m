%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = ip_edge_hysteresis(strong, weak)
  strong = logical(strong);
  weak = logical(weak) | strong;
  bw = false(size(strong));
  if ~any(strong(:)) || ~any(weak(:))
    return
  end
  cc = bwconncomp(weak, 8);
  strong = strong(:);
  for k = 1:cc.NumObjects
    idx = cc.PixelIdxList{k};
    if any(strong(idx))
      bw(idx) = true;
    end
  end
end
%=============================================================================
