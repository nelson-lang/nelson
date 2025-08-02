%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bandwidth(varargin)
  narginchk(1, 2);
  A = varargin{1};
  if (~isnumeric (A) && ~islogical (A) || ndims (A) != 2)
    error(_('A must be a 2-D numeric or logical matrix.'));
  end
  if (nargin == 1)
    nargoutchk(0, 2);
    lower = 0;
    upper = 0;
    [i, j] = find(A);        
    if ~isempty(i)
      lower = max (0, max (i - j));
      upper = max (0, max (j - i));
    end
    varargout{1} = lower;
    varargout{2} = upper;
  else
    nargoutchk(0, 1);
    type = varargin{2};
    if ~(strcmp (type, 'lower') || strcmp (type, 'upper'))
      error(_('TYPE must be ''lower'' or ''upper''.'));
    end
    [i, j] = find (A);
    if isempty(i)
      varargout{1} = 0;
    elseif (strcmp (type, 'lower'))
      varargout{1} = max (0, max (i - j));
    else
      varargout{1} = max (0, max (j - i));
    end
  end
end
%=============================================================================
