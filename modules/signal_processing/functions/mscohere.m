%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mscohere(varargin)
  narginchk(2, 7);
  nargoutchk(0, 2);
  x = varargin{1};
  y = varargin{2};
  args = varargin(3:end);
  [pxy, f] = cpsd(x, y, args{:});
  [pxx] = pwelch(x, args{:});
  [pyy] = pwelch(y, args{:});
  n = min([length(pxy), length(pxx), length(pyy)]);
  cxy = abs(pxy(1:n)) .^ 2 ./ (pxx(1:n) .* pyy(1:n));
  if nargout == 0
    plot(f(1:n), cxy);
  else
    varargout{1} = cxy;
    if nargout > 1
      varargout{2} = f(1:n);
    end
  end
end
%=============================================================================
