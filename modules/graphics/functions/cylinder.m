%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cylinder(varargin)
  narginchk(0,3);
  nargoutchk(0,3);
  
  if (nargin == 3)
    cax = varargin{1};
    args = {varargin{1}, varargin{2}};
    nargs = 2;
  elseif (nargin == 2)
    if isgraphics(varargin{1})
      cax = varargin{1};
      args = {varargin{2}};
      nargs = 1;
    else
      cax = [];
      args = {varargin{1}, varargin{2}};
      nargs = 2;
    end
  elseif (nargin == 1)
    if isgraphics(varargin{1})
      cax = varargin{1};
      args = {};
      nargs = 0;
    else
      cax = [];
      args = {varargin{1}};
      nargs = 1;
    end
  else
    cax = [];
    args = {};
    nargs = 0;
  end
  n = 20;
  r = [1 1]';
  if (nargs > 0)
    r = args{1};
  end
  if (nargs > 1)
    n = args{2};
  end
  r = r(:);
  m = length(r);
  if (m == 1)
    r = [r; r];
    m = 2;
  end
  theta = (0:n) / n * 2 * pi;
  sintheta = sin(theta);
  sintheta(n + 1) = 0;
  x = r * cos(theta);
  y = r * sintheta;
  z = (0:m-1)' / (m-1) * ones(1, n + 1);
  
  if (nargout == 0)
    cax = newplot(cax);
    surf(x, y, z, 'Parent', cax);
  else
    varargout{1} = x;
    varargout{2} = y;
    varargout{3} = z;
  end
end
%=============================================================================
