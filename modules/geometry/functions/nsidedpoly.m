%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = nsidedpoly(n, varargin)
  narginchk(1, inf);
  if ~(isscalar(n) && isreal(n) && n >= 3 && n == floor(n))
    error('Nelson:nsidedpoly:invalidN', 'N must be an integer greater than or equal to 3.');
  end
  center = [0, 0];
  radius = [];
  sideLength = [];
  k = 1;
  while k <= numel(varargin)
    if k + 1 > numel(varargin)
      error('Nelson:nsidedpoly:nameValuePairs', 'Name-value arguments must be paired.');
    end
    name = lower(char(varargin{k}));
    value = varargin{k + 1};
    switch name
      case 'center'
        center = value;
      case 'radius'
        radius = value;
      case 'sidelength'
        sideLength = value;
      otherwise
        error('Nelson:nsidedpoly:unknownParameter', 'Unsupported "%s" option.', name);
    end
    k = k + 2;
  end
  if ~isempty(sideLength)
    radius = sideLength / (2 * sin(pi / n));
  elseif isempty(radius)
    % default: a regular polygon of unit area
    radius = sqrt(2 / (n * sin(2 * pi / n)));
  end
  theta = (0:n - 1)' / n * 2 * pi + pi / 2;
  x = center(1) + radius * cos(theta);
  y = center(2) + radius * sin(theta);
  p = polyshape(x, y);
end
%=============================================================================
