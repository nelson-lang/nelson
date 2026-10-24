%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function w = chebwin(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  m = varargin{1};
  if nargin < 2
    ripple = 100;
  else
    ripple = varargin{2};
  end
  signalValidateWindowLength(m);
  if isempty(m)
    w = zeros(0, 1);
  elseif m == 1
    w = 1;
  else
    tg = 10 ^ (abs(ripple) / 20);
    beta = cosh(acosh(tg) / (m - 1));
    k = (0:m - 1)';
    x = beta * cos(pi * k / m);
    p = signalChebpoly(m - 1, x);
    if mod(m, 2) == 1
      w = real(fft(p));
      w = w(1:m);
      w = [w((m + 1) / 2:m); w(1:(m - 1) / 2)];
    else
      p = p .* exp(complex(0, 1) * pi * k / m);
      w = real(fft(p));
      w = [w(m / 2 + 1:m); w(1:m / 2)];
    end
    w = [w(2:end); w(1)];
    w = w ./ max(w);
  end
end
%=============================================================================
