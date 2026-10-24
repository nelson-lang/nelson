%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = stepz(varargin)
  narginchk(1, 4);
  nargoutchk(0, 2);
  [h, t] = impz(varargin{:});
  s = cumsum(h);
  if nargout == 0
    stem(t, s);
  else
    varargout{1} = s;
    if nargout > 1
      varargout{2} = t;
    end
  end
end
%=============================================================================
