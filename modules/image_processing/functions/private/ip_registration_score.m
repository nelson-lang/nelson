%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function score = ip_registration_score(moving, fixed, T, metricType, interpolation)
  warped = imwarp(moving, T, 'Interpolation', interpolation, ...
    'OutputView', [size(fixed, 1), size(fixed, 2)]);
  if strcmp(metricType, 'meansquares')
    d = warped - fixed;
    score = -mean(d(:) .^ 2);
  else
    a = warped(:) - mean(warped(:));
    b = fixed(:) - mean(fixed(:));
    denom = sqrt(sum(a .^ 2) * sum(b .^ 2));
    if denom == 0
      score = 0;
    else
      score = sum(a .* b) / denom;
    end
  end
end
%=============================================================================
