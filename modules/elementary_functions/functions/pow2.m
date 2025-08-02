
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = pow2 (varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if (nargin == 2)
    X = varargin{1};
    E = varargin{2};
    doComplexWarning = false;
    if ~isreal(X)
      X = real(X);
      doComplexWarning = true;
    end
    if ~isreal(E)
      E = real(E);
      doComplexWarning = true;
    end
    R = X .* (2 .^ E);
    if doComplexWarning
      warning('Nelson:pow2:ignoredImagPart', _('Imaginary part is ignored.'));
    end
  else
    Y = varargin{1};
    R = 2 .^ Y;
  end
end
%=============================================================================
