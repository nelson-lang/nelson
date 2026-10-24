%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = signalBesseli0(x)
  y = zeros(size(x));
  term = ones(size(x));
  k = 0;
  while max(abs(term(:))) > eps()
    y = y + term;
    k = k + 1;
    term = term .* ((x ./ 2) .^ 2) ./ (k ^ 2);
    if k > 60
      break;
    end
  end
end
%=============================================================================
