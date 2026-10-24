%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [bw, thresh] = ip_edge_log(img, thresh, sigma)
  ip_validate_edge_threshold(thresh, false);
  thresh = double(thresh);
  sigma = ip_validate_edge_sigma(sigma, 2);
  filterSize = 2 * ceil(3 * sigma) + 1;
  response = imfilter(img, fspecial('log', [filterSize filterSize], sigma), 'replicate');
  if isempty(thresh)
    thresh = 0.75 * mean(abs(response(:)));
  end
  bw = false(size(img));
  for dy = -1:1
    for dx = -1:1
      if dy == 0 && dx == 0
        continue
      end
      shifted = padarray(response, [1 1], 'replicate');
      shifted = shifted(2 + dy:end - 1 + dy, 2 + dx:end - 1 + dx);
      crossing = (response > 0 & shifted < 0) | (response < 0 & shifted > 0);
      touchingZero = response == 0 & shifted ~= 0;
      bw = bw | ((crossing | touchingZero) & abs(response - shifted) > thresh);
    end
  end
end
%=============================================================================
