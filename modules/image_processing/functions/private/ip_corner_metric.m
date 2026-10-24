%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function metric = ip_corner_metric(img, method, filterSize, sensitivityFactor)
  dx = imfilter(img, [-1 0 1] / 2, 'replicate');
  dy = imfilter(img, [-1; 0; 1] / 2, 'replicate');
  g = fspecial('gaussian', [filterSize filterSize], max(filterSize / 6, eps));
  a = imfilter(dx .^ 2, g, 'replicate');
  b = imfilter(dy .^ 2, g, 'replicate');
  c = imfilter(dx .* dy, g, 'replicate');
  detM = a .* b - c .^ 2;
  traceM = a + b;
  if strcmp(method, 'harris')
    metric = detM - sensitivityFactor * traceM .^ 2;
  else
    disc = max(traceM .^ 2 - 4 * detM, 0);
    metric = (traceM - sqrt(disc)) / 2;
  end
end
%=============================================================================
