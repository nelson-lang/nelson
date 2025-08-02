%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = polyfit(x, y, n)
  if (numel(x) ~= numel(y))
    error('Nelson:polyfit:XYSizeMismatch', _('The first two inputs must have the same number of elements.'));
  end
  x = x(:);
  y = y(:);
  A = zeros(size(x, 1), n + 1);
  xp = x*0+1;
  for i =(n+1):-1:1
    A(:, i) = xp;
    xp = xp .* x;
  end
  p = (pinv(A) * y)';
end
%=============================================================================
