%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = flag(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);    
  if nargin < 1
    R = groot();
    f = R.CurrentFigure;
    if isempty(f)
      m = size(R.DefaultFigureColormap, 1);
    else
      m = size(f.Colormap, 1);
    end
  else
    m = varargin{1};
  end
  f = [1 0 0;
  1 1 1;
  0 0 1;
  0 0 0];
  
  e = ones(ceil(m / 4), 1);
  map = kron(e, f);
  varargout{1} = map(1:m,:);
end