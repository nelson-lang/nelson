%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = randomSparse(generator, varargin)
  if nargin == 2
    R = randomSparseLike(generator, varargin{1});
  elseif nargin == 4
    R = randomSparseWithDensity(generator, varargin{1}, varargin{2}, varargin{3});
  else
    error('Nelson:sparse:invalidNumberInputArguments2', 'Invalid number of input arguments.');
  end
end
%=============================================================================
function R = randomSparseWithDensity(generator, mInput, nInput, densityInput)
  rowCount = double(mInput);
  columnCount = double(nInput);
  density = min(double(densityInput), 1);
  requestedCount = round(rowCount * columnCount * density);
  if requestedCount == 0
    R = sparse([], [], [], rowCount, columnCount);
    return
  end

  rowIndex = randi(rowCount, requestedCount, 1);
  columnIndex = randi(columnCount, requestedCount, 1);
  uniquePairs = unique([rowIndex columnIndex], 'rows');
  rowIndex = uniquePairs(:, 1);
  columnIndex = uniquePairs(:, 2);
  R = sparse(rowIndex, columnIndex, generator(length(rowIndex), 1), rowCount, columnCount);
end
%=============================================================================
function R = randomSparseLike(generator, S)
  if ~ismatrix(S)
    error('Nelson:randomSparse:invalidInput', 'Input must be a matrix.');
  end
  rowCount = size(S, 1);
  columnCount = size(S, 2);
  [rowIndex, columnIndex] = find(S); %#ok<NLS0004>
  R = sparse(rowIndex, columnIndex, generator(numel(rowIndex), 1), rowCount, columnCount);
end
%=============================================================================
