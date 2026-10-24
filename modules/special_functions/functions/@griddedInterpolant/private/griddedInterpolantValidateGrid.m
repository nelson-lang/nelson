%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [gridVectors, values] = griddedInterpolantValidateGrid(gridVectors, values)
  if ~iscell(gridVectors)
    error('Nelson:griddedInterpolant:invalidGridVectors', 'GridVectors must be a cell array of grid vectors.');
  end
  if ~isnumeric(values) || ~isreal(values) || issparse(values)
    error('Nelson:griddedInterpolant:invalidSampleValues', 'Sample values must be a real full numeric array.');
  end
  nd = numel(gridVectors);
  if nd == 0
    error('Nelson:griddedInterpolant:noGridVector', 'At least one grid vector is required.');
  end
  if nd == 1
    expected = numel(values);
  else
    valueSize = size(values);
    if numel(valueSize) < nd
      valueSize(end + 1:nd) = 1;
    end
  end
  for dim = 1:nd
    g = gridVectors{dim};
    if ~isnumeric(g) || ~isreal(g) || issparse(g) || ~isvector(g) || isempty(g)
      error('Nelson:griddedInterpolant:gridVectorsMustBeNumeric', 'Grid vectors must be real full numeric vectors.');
    end
    if nd == 1
      expectedDim = expected;
    else
      expectedDim = valueSize(dim);
    end
    if numel(g) ~= expectedDim
      error('Nelson:griddedInterpolant:invalidGrid', 'Sample points vector corresponding to grid dimension %d must contain %d elements.', dim, expectedDim);
    end
    dg = diff(g(:));
    if any(dg <= 0)
      error('Nelson:griddedInterpolant:samplePointsNotSorted', 'Sample points must be sorted in ascending order.');
    end
    gridVectors{dim} = double(g);
  end
end
%=============================================================================
