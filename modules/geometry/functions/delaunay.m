%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tri = delaunay(varargin)
  narginchk(1, 4);
  points = parseDelaunayInputs(varargin{:});
  tri = delaunayn(points);
end
%=============================================================================
function points = parseDelaunayInputs(varargin)
  if length(varargin) == 1
    points = varargin{1};
  elseif length(varargin) == 2
    points = [varargin{1}(:), varargin{2}(:)];
  elseif length(varargin) == 3
    points = [varargin{1}(:), varargin{2}(:), varargin{3}(:)];
  else
    error('Nelson:geometry:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
  if ~isnumeric(points) || size(points, 2) < 2
    error('Nelson:geometry:pointsNumericMatrixLeastTwoColumns', '%s must be a numeric matrix with at least two columns.', 'Points');
  end
end
%=============================================================================
