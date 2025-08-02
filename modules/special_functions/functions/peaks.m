%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = peaks (varargin)
  narginchk(0, 2);
  nargoutchk(0, 3);
  
  if nargin == 0
    dx = 1 / 8;
    [x, y] = meshgrid(-3 : dx : 3);
  elseif nargin == 1
    param1 = varargin{1};
    if length(param1) == 1
      [x, y] = meshgrid(linspace(-3, 3, param1));
    else
      [x, y] = meshgrid(param1, param1);     
    end
  else
    x = varargin{1};
    y = varargin{2};
  end
  
  part1 = 3 * (1 - x) .^ 2 .* exp(-(x .^ 2) - (y + 1) .^ 2);
  part2 = 10*(x / 5 - x .^ 3 - y .^ 5) .* exp(-x .^ 2 - y .^ 2);
  part3 = 1 / 3 *exp(-(x + 1) .^ 2 - y .^ 2);
  z =  part1 - part2 - part3;
  
  if nargout == 0
    disp(' ')
    disp('z =  3*(1-x).^2.*exp(-(x.^2) - (y+1).^2) ... ')
    disp('   - 10*(x/5 - x.^3 - y.^5).*exp(-x.^2-y.^2) ... ')
    disp('   - 1/3*exp(-(x+1).^2 - y.^2) ')
    disp(' ')
    surf(x, y, z);
    axis('tight')
    xlabel('x');
    ylabel('y');
    title(_('Peaks'));
  elseif nargout == 1
    varargout{1} = z;
  else
    varargout{1} = x;
  end
  
  if nargout > 1
    varargout{2} = y;
  end
  if nargout > 2
    varargout{3} = z;
  end
end
