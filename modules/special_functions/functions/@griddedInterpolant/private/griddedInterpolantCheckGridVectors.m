%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function gridVectors = griddedInterpolantCheckGridVectors(value)
  if ~iscell(value)
    error('Nelson:griddedInterpolant:invalidGridVectors', 'GridVectors must be a cell array of grid vectors.');
  end
  gridVectors = reshape(value, 1, numel(value));
  for dim = 1:numel(gridVectors)
    g = gridVectors{dim};
    if ~isnumeric(g) || ~isreal(g) || issparse(g) || ~isvector(g) || isempty(g)
      error('Nelson:griddedInterpolant:gridVectorsMustBeNumeric', 'Grid vectors must be real full numeric vectors.');
    end
    if any(diff(g(:)) <= 0)
      error('Nelson:griddedInterpolant:samplePointsNotSorted', 'Sample points must be sorted in ascending order.');
    end
    gridVectors{dim} = double(g);
  end
end
%=============================================================================
