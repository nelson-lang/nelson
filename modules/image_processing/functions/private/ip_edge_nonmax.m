%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nms = ip_edge_nonmax(mag, angle)
  nms = zeros(size(mag));
  height = size(mag, 1);
  width = size(mag, 2);
  if height < 3 || width < 3
    return
  end
  ux = cos(angle);
  uy = sin(angle);
  for y = 2:height - 1
    for x = 2:width - 1
      forwardY = y + uy(y, x);
      forwardX = x + ux(y, x);
      backwardY = y - uy(y, x);
      backwardX = x - ux(y, x);
      y1 = min(max(floor(forwardY), 1), height - 1);
      x1 = min(max(floor(forwardX), 1), width - 1);
      dy = forwardY - y1;
      dx = forwardX - x1;
      before = (1 - dy) * ((1 - dx) * mag(y1, x1) + dx * mag(y1, x1 + 1)) ...
        + dy * ((1 - dx) * mag(y1 + 1, x1) + dx * mag(y1 + 1, x1 + 1));
      y1 = min(max(floor(backwardY), 1), height - 1);
      x1 = min(max(floor(backwardX), 1), width - 1);
      dy = backwardY - y1;
      dx = backwardX - x1;
      after = (1 - dy) * ((1 - dx) * mag(y1, x1) + dx * mag(y1, x1 + 1)) ...
        + dy * ((1 - dx) * mag(y1 + 1, x1) + dx * mag(y1 + 1, x1 + 1));
      if mag(y, x) >= before && mag(y, x) >= after
        nms(y, x) = mag(y, x);
      end
    end
  end
end
%=============================================================================
