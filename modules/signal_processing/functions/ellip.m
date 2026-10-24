%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ellip(varargin)
  narginchk(4, 5);
  nargoutchk(0, 3);
  n = varargin{1};
  rp = varargin{2};
  rs = varargin{3};
  wn = varargin{4};
  ftype = '';
  if nargin == 5
    ftype = varargin{5};
  end
  mustBeInteger(n);
  mustBePositive(n);
  [z, p] = signalEllipPrototype(n, rp, rs);
  [z, p, k, b, a] = signalIirPrototypeToDigital(z, p, n, rp, wn, ftype);
  if nargout <= 2
    varargout{1} = b;
    if nargout > 1
      varargout{2} = a;
    end
  else
    varargout{1} = z;
    varargout{2} = p;
    varargout{3} = k;
  end
end
%=============================================================================
