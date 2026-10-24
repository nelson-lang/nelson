%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [level, em] = graythresh(img)
  narginchk(1, 1);
  nargoutchk(0, 2);
  counts = imhist(img, 256);
  p = counts / sum(counts);
  omega = cumsum(p);
  mu = cumsum(p .* (1:256)');
  muT = mu(256);
  sigma = (muT * omega - mu) .^ 2 ./ max(omega .* (1 - omega), eps);
  [~, idx] = max(sigma);
  level = (idx - 1) / 255;
  variance = sum((((1:256)' - muT) .^ 2) .* p);
  if variance == 0
    em = 0;
  else
    em = max(sigma) / variance;
  end
  em = min(max(em, 0), 1);
end
%=============================================================================
