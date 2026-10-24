%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function w = triang(m)
  narginchk(1, 1);
  nargoutchk(0, 1);
  signalValidateWindowLength(m);
  if isempty(m)
    w = zeros(0, 1);
  elseif mod(m, 2) == 0
    n = (1:m)';
    w = (2 / m) * (m / 2 - abs(n - (m + 1) / 2));
  else
    n = (1:m)';
    w = (2 / (m + 1)) * ((m + 1) / 2 - abs(n - (m + 1) / 2));
  end
end
%=============================================================================
