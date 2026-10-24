%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function t = toeplitz(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);

  columnSeed = varargin{1};
  if (nargin == 1)
    if ~isvector(columnSeed)
      error('Nelson:validators:mustBeVector', '%s must be a vector.', 'Input argument');
    end
    columnSeed(1) = conj(columnSeed(1));
    rowSeed = columnSeed;
    columnSeed = conj(columnSeed);
  else
    rowSeed = varargin{2};
  end
  if ~(isnumeric(columnSeed) || isnumeric(rowSeed))
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Input arguments');
  end
  if ~isempty(columnSeed) && ~isempty(rowSeed) && ~isequaln(rowSeed(1), columnSeed(1))
    warning('Nelson:toeplitz:columnWinsDiagonalConflict', 'Column wins diagonal conflict.');
  end
  columnValues = columnSeed(:);
  rowValues = rowSeed(:);
  rowCount = length(columnValues);
  columnCount = length(rowValues);
  lookupValues = [rowValues(columnCount:-1:2); columnValues];
  rowIndex = (1:rowCount).';
  columnIndex = 1:columnCount;
  t = lookupValues(columnCount + rowIndex - columnIndex);
  if ~isempty(t) && isrow(rowIndex - columnIndex)
    t = t.';
  end
end
%=============================================================================
