%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tfestimate(varargin)
  narginchk(2, 7);
  nargoutchk(0, 2);
  x = varargin{1};
  y = varargin{2};
  args = varargin(3:end);
  [pyx, f] = cpsd(y, x, args{:});
  [pxx] = pwelch(x, args{:});
  n = min(length(pyx), length(pxx));
  txy = pyx(1:n) ./ pxx(1:n);
  tolerance = 100 * eps() * max(1, max(abs(txy)));
  realValues = abs(imag(txy)) <= tolerance;
  txy(realValues) = real(txy(realValues));
  if nargout == 0
    plot(f(1:n), abs(txy));
  else
    varargout{1} = txy;
    if nargout > 1
      varargout{2} = f(1:n);
    end
  end
end
%=============================================================================
