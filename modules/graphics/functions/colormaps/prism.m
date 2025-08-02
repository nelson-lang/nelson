%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = prism(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);   
  nargs =  nargin + nargout; 
  if (nargs == 0)
    ax = gca();
    h = ax.Children;
    m = length(h);
  elseif (nargin == 0)
    R = groot();
    f = R.CurrentFigure;
    if isempty(f)
      m = size(R.DefaultFigureColormap, 1);
    else
      m = size(f.Colormap,1);
    end
  else
    m = varargin{1};
  end
  R = [1 0 0;
  1 .5 0;
  1 1 0;
  0 1 0;
  0 0 1;
  2/3 0 1];
  
  e = ones(ceil(m / 6), 1);
  R = kron(e, R);
  R = R(1:m, :);
  
  if (nargs) == 0
    for k = 1:m
      H = h(k);
      if strcmp(H.Type, 'line')
        H.Color = R(k,:);
      end
    end
  else
    map = R;
  end
  varargout{1} = map;
end
%=============================================================================
