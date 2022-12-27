%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = spring(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);    
  if nargin ==  0
    gr  = groot();
    f = gr.CurrentFigure; 
    if isempty(f)
      n = 64;
    else
      n = size(f.Colormap,1);
    end
  else
    n = varargin{1};
  end
  
  if (n == 1)
    varargout{1} = [1, 0, 0];
  else
    if (n <= 0)
      varargout{1} = zeros (0, 3);
    else
      r = (0:n-1)' / max(n-1, 1);
      varargout{1} = [ones(n,1), r, (1 - r)];
    end
  end
end
%=============================================================================
