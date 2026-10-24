%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function metric = ip_fast_corner_metric(img, minContrast)
  [rows, cols] = size(img);
  metric = zeros(rows, cols);
  offsets = [-3 0; -3 1; -2 2; -1 3; 0 3; 1 3; 2 2; 3 1; ...
    3 0; 3 -1; 2 -2; 1 -3; 0 -3; -1 -3; -2 -2; -3 -1];
  for r = 4:rows - 3
    for c = 4:cols - 3
      center = img(r, c);
      ring = zeros(1, 16);
      for k = 1:16
        ring(k) = img(r + offsets(k, 1), c + offsets(k, 2));
      end
      bright = ring > center + minContrast;
      dark = ring < center - minContrast;
      if ip_has_fast_arc(bright) || ip_has_fast_arc(dark)
        metric(r, c) = max(abs(ring - center));
      end
    end
  end
end
%=============================================================================
