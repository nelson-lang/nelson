%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = impz(varargin)
  narginchk(1, 4);
  nargoutchk(0, 2);
  b = varargin{1};
  a = 1;
  n = [];
  fs = [];
  if nargin >= 2
    a = varargin{2};
  end
  if nargin >= 3
    n = varargin{3};
  end
  if nargin == 4
    fs = varargin{4};
  end
  if isempty(n)
    n = impzlength(b, a);
  end
  impulse = [1; zeros(double(n) - 1, 1)];
  h = filter(b, a, impulse);
  if isempty(fs)
    t = (0:double(n) - 1)';
  else
    t = (0:double(n) - 1)' ./ double(fs);
  end
  if nargout == 0
    stem(t, h);
  else
    varargout{1} = h;
    if nargout > 1
      varargout{2} = t;
    end
  end
end
%=============================================================================
