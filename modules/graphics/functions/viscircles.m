%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = viscircles(varargin)
  % h = viscircles(centers, radii)
  % h = viscircles(ax, centers, radii)
  % h = viscircles(..., 'Color', c, 'LineWidth', w, 'EnhanceVisibility', tf)
  narginchk(2, inf);
  nargoutchk(0, 1);
  args = varargin;
  ax = [];
  if isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args(1) = [];
  end
  if numel(args) < 2
    error('Nelson:viscircles:notEnoughInputs', 'CENTERS and RADII are required.');
  end
  centers = args{1};
  radii = args{2};
  if size(centers, 2) ~= 2
    error('Nelson:viscircles:invalidCenters', 'CENTERS must be an N-by-2 matrix.');
  end
  numCircles = size(centers, 1);
  radii = radii(:);
  if isscalar(radii)
    radii = repmat(radii, numCircles, 1);
  elseif numel(radii) ~= numCircles
    error('Nelson:viscircles:invalidRadii', ...
      'RADII must be a scalar or have one value per circle.');
  end
  color = [1 0 0];
  lineWidth = 2;
  k = 3;
  while k <= numel(args)
    if k + 1 > numel(args)
      error('Nelson:viscircles:nameValuePairs', 'Name-value arguments must be paired.');
    end
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'color'
        color = value;
      case 'linewidth'
        lineWidth = value;
      case 'enhancevisibility'
        % The white backing outline is not drawn; accepted for compatibility.
      otherwise
        error('Nelson:viscircles:unknownParameter', 'Unsupported "%s" option.', name);
    end
    k = k + 2;
  end
  if isempty(ax)
    ax = gca();
  end
  wasHold = ishold(ax);
  hold(ax, 'on');
  group = hggroup('Parent', ax);
  theta = linspace(0, 2 * pi, 100);
  cosTheta = cos(theta);
  sinTheta = sin(theta);
  for i = 1:numCircles
    line(centers(i, 1) + radii(i) * cosTheta, centers(i, 2) + radii(i) * sinTheta, ...
      'Color', color, 'LineWidth', lineWidth, 'Parent', group);
  end
  if ~wasHold
    hold(ax, 'off');
  end
  if nargout > 0
    varargout{1} = group;
  end
end
%=============================================================================
