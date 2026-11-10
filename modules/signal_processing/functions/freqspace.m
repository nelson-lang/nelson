%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [f1, f2] = freqspace(N, flag)
  % frequency points for frequency responses, normalized so that 1 is the Nyquist frequency
  narginchk(1, 2);
  if nargin < 2
    flag = '';
  elseif ~((ischar(flag) || isstring(flag)) && any(strcmpi(char(flag), {'whole', 'meshgrid'})))
    error('Nelson:freqspace:invalidOption', 'Option must be ''whole'' or ''meshgrid''.');
  end
  flag = lower(char(flag));
  if ~(isnumeric(N) && isreal(N) && any(numel(N) == [1 2]) && all(N >= 1))
    error('Nelson:freqspace:invalidSize', 'N must be a positive scalar or a two-element vector.');
  end
  N = double(N);
  if nargout <= 1
    n = N(1);
    if strcmp(flag, 'whole')
      f1 = (0:n - 1) * (2 / n);
    else
      f1 = (0:floor(n / 2)) * (2 / n);
    end
    return
  end
  if isscalar(N)
    m = N;
    n = N;
  else
    m = N(1);
    n = N(2);
  end
  f1 = ((0:n - 1) - floor(n / 2)) * (2 / n);
  f2 = ((0:m - 1) - floor(m / 2)) * (2 / m);
  if strcmp(flag, 'meshgrid')
    [f1, f2] = meshgrid(f1, f2);
  end
end
%=============================================================================
