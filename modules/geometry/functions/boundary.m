%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [k, v] = boundary(varargin)
  nargoutchk(0, 2);
  narginchk(1, 4);
  shrink = 0.5;
  if isnumeric(varargin{end}) && isscalar(varargin{end}) && length(varargin) > 1
    shrink = varargin{end};
    varargin = varargin(1:end - 1);
  end
  points = parseBoundaryPoints(varargin{:});
  shape = alphaShape(points, alphaFromShrink(points, shrink));
  facets = boundaryFacets(shape);
  if nargout == 0
    plot(shape);
    return
  end
  k = facets;
  if nargout > 1
    if size(points, 2) == 2
      v = area(shape);
    else
      v = volume(shape);
    end
  end
end
%=============================================================================
function points = parseBoundaryPoints(varargin)
  if length(varargin) == 1
    points = varargin{1};
  elseif length(varargin) == 2
    points = [varargin{1}(:), varargin{2}(:)];
  elseif length(varargin) == 3
    points = [varargin{1}(:), varargin{2}(:), varargin{3}(:)];
  else
    error('Nelson:geometry:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function alpha = alphaFromShrink(points, shrink)
  if shrink >= 1
    alpha = Inf;
  else
    span = max(points, [], 1) - min(points, [], 1);
    alpha = max(span) * max(shrink, eps);
  end
end
%=============================================================================
