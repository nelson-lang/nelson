%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = sprand(varargin)
  narginchk(1,3);
  switch nargin()
    case 1
      R = sprand_case1(varargin{1});
    case 2
      error(_('Invalid number of input arguments.'));
    case 3
      R = sprand_case3(varargin{1}, varargin{2}, varargin{3});
    otherwise
      error(_('Invalid number of input arguments.'));
    end
end
%=============================================================================
function R = sprand_case3(m_in, n_in, density_in)
  numRows = double(m_in);
  numCols = double(n_in);
  density = double(density_in);
  numNonZerosWanted = round(numRows * numCols * min(density,1));
  if numNonZerosWanted > 0
    rowIndices = randi(numRows, numNonZerosWanted, 1);
    colIndices = randi(numCols, numNonZerosWanted, 1);
    pairs = unique([rowIndices colIndices], 'rows');
    rowIndices = pairs(:,1);
    colIndices = pairs(:,2);
    randomValues = rand(length(rowIndices), 1, 'double');
    R = sparse(rowIndices, colIndices, randomValues, numRows, numCols);
  else
    R = sparse([], [], [], numRows, numCols);
  end
end
%=============================================================================
function R = sprand_case1(S)
  if ~ismatrix(S)
    error(_('Input must be a matrix.'));
  end
  [numRows, numCols] = size(S);
  [rowIndices, colIndices] = find(S);
  R = sparse(rowIndices, colIndices, rand(numel(rowIndices), 1, 'double'), numRows, numCols);
end
%=============================================================================
