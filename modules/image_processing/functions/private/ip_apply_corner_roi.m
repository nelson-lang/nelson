%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mask = ip_apply_corner_roi(mask, roi)
  if isempty(roi)
    return
  end
  [rows, cols] = size(mask);
  x1 = max(1, floor(roi(1)));
  y1 = max(1, floor(roi(2)));
  x2 = min(cols, ceil(roi(1) + roi(3)));
  y2 = min(rows, ceil(roi(2) + roi(4)));
  keep = false(rows, cols);
  if x1 <= x2 && y1 <= y2
    keep(y1:y2, x1:x2) = true;
  end
  mask = mask & keep;
end
%=============================================================================
