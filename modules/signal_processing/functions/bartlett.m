%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = bartlett(varargin)
  % Oppenheim, Alan V., Ronald W. Schafer, and John R. Buck. Discrete-Time Signal Processing. Upper Saddle River, NJ: Prentice Hall, 1999, pp.
  narginchk(1, 1);
  m = varargin{1};
  isPositiveIntegerOrEmpty = isempty(m) || (isscalar(m) && (m == fix (m)) && (m > 0));
  if ~isPositiveIntegerOrEmpty
    error(_('M must be a positive integer.'));
  end
  if isempty(m)
    c = zeros(0, 1);
  elseif (m ~= 1)
    m = m - 1;
    n = fix (m / 2);
    c = [2*(0:n)/m, 2-2*(n+1:m)/m]';
  else
    c = 1;
  end
end
%=============================================================================
