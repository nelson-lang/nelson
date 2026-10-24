%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = impzlength(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  b = varargin{1};
  a = 1;
  tolerance = 5e-5;
  if nargin == 2
    a = varargin{2};
  end
  if nargin == 3
    a = varargin{2};
    tolerance = varargin{3};
  end
  if isfir(b, a)
    n = length(b);
  else
    n = 64;
    poles = roots(a);
    if ~isempty(poles)
      radius = max(abs(poles));
      if radius > 0 && radius < 1
        n = max(1, floor(log(tolerance) / log(radius)));
      end
    end
  end
end
%=============================================================================
