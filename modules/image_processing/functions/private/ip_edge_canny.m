%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [bw, thresh] = ip_edge_canny(img, thresh, sigma)
  ip_validate_edge_threshold(thresh, true);
  thresh = double(thresh);
  sigma = ip_validate_edge_sigma(sigma, sqrt(2));
  filterSize = 2 * ceil(3 * sigma) + 1;
  smooth = imfilter(img, fspecial('gaussian', [filterSize filterSize], sigma), 'replicate');
  gx = imfilter(smooth, fspecial('sobel')', 'replicate');
  gy = imfilter(smooth, fspecial('sobel'), 'replicate');
  mag = sqrt(gx .^ 2 + gy .^ 2);
  nms = ip_edge_nonmax(mag, atan2(gy, gx));
  if isempty(thresh)
    positive = nms(nms > 0);
    if isempty(positive)
      thresh = [0 0];
      bw = false(size(img));
      return
    end
    high = graythresh(positive);
    low = 0.4 * high;
    thresh = [low high];
  elseif isscalar(thresh)
    thresh = [0.4 * thresh thresh];
  else
    thresh = reshape(thresh, 1, 2);
  end
  nonzero = nms > 0;
  strong = nonzero & nms >= thresh(2);
  weak = nonzero & nms >= thresh(1);
  bw = ip_edge_hysteresis(strong, weak);
end
%=============================================================================
