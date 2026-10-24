%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = peaks(varargin)
  narginchk(0, 2);
  nargoutchk(0, 3);

  grid = peaksGrid(varargin{:});
  z = peaksSurface(grid.x, grid.y);

  if nargout == 0
    displayPeaks(grid.x, grid.y, z);
  elseif nargout == 1
    varargout{1} = z;
  else
    varargout{1} = grid.x;
    varargout{2} = grid.y;
    if nargout > 2
      varargout{3} = z;
    end
  end
end
%=============================================================================
function grid = peaksGrid(varargin)
  if nargin == 0
    spacing = 1 / 8;
    [grid.x, grid.y] = meshgrid(-3:spacing:3);
  elseif nargin == 1
    gridArgument = varargin{1};
    if isscalar(gridArgument)
      [grid.x, grid.y] = meshgrid(linspace(-3, 3, gridArgument));
    else
      [grid.x, grid.y] = meshgrid(gridArgument, gridArgument);
    end
  else
    grid.x = varargin{1};
    grid.y = varargin{2};
  end
end
%=============================================================================
function z = peaksSurface(x, y)
  leftPeak = 3 .* (1 - x) .^ 2 .* exp(-x .^ 2 - (y + 1) .^ 2);
  saddle = 10 .* (x ./ 5 - x .^ 3 - y .^ 5) .* exp(-x .^ 2 - y .^ 2);
  rightPeak = exp(-(x + 1) .^ 2 - y .^ 2) ./ 3;
  z = leftPeak - saddle - rightPeak;
end
%=============================================================================
function displayPeaks(x, y, z)
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
end
%=============================================================================
