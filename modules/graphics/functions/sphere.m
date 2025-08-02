%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = sphere(varargin)
  narginchk(0, 2);
  nargoutchk(0, 3);
  n = 20;
  if nargin == 0
    ax = [];
  else 
    if nargin == 1
      v = varargin{1};
      if isgraphics(v)
        ax = v;
      else
        ax = [];
        n = v;
      end
    else
      ax = varargin{1};
      n = varargin{2};
    end
  end
  phi = (-n:2:n)' / n * pi / 2;
  theta = (-n:2:n) / n * pi;
  cosphi = cos(phi);
  cosphi(n + 1) = 0;
  cosphi(1) = 0;
  sintheta = sin(theta);
  sintheta(n + 1) = 0;
  sintheta(1) = 0;
  
  x = cosphi * cos(theta);
  y = cosphi * sintheta;
  z = sin(phi) * ones(1, n + 1);
  
  if nargout == 0
    ax = newplot(ax);
    surf(x, y, z, 'Parent', ax);
  else
    varargout{1} = x;
    varargout{2} = y;
    varargout{3} = z;
  end
end
%=============================================================================
