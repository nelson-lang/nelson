%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = size(varargin)
  narginchk(1, 2);
  nargoutchk(0, 2);
  [m, n] =  size(varargin{1}.Numerator);
  if (nargout == 0 && nargin == 1)
    disp(sprintf(_('Transfer function with %d outputs and %d inputs.'), m, n));
  else
    if (nargin == 1)
      if (nargout == 1)
        varargout{1} = [m, n];
      else
        varargout{1} = m;
        varargout{2} = n;
      end
    else
      pos = varargin{2};
      if (pos < 1)
        error(_('Second argument must be a positive integer.'));
      end
      switch pos
        case  1
          varargout{1} = m;
        case 2
          varargout{1} = n;
        otherwise
          varargout{1} = 1;
        end
      end
    end
  end
  %=============================================================================
  
