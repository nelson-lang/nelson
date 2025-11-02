%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = moler(nSize, alphaVal, className)
  % moler  Construct Moler matrix A = T'*T where T is a specific triangular matrix.
  %   A = moler(nSize) or A = moler(nSize, alphaVal, className)
  %   - nSize: scalar or 2-element vector for [m,n] passed to triw.
  %   - alphaVal: scalar multiplier used in triw (default: -1).
  %   - className: optional numeric class for output (e.g. 'double').
  if isempty(alphaVal)
    alphaVal = -ones(className);
  end
  % Build T and compute A = T' * T
  T = triw(nSize, alphaVal, [], className);
  A = T' * T;
end
%=============================================================================
function T = triw(nSize, alphaVal, upperBand, className)
  % triw  Build a (possibly rectangular) lower-triangular matrix T.
  %   T = triw(nSize, alphaVal, upperBand, className)
  %   - nSize: scalar N or vector [m n] (first and last elements used)
  %   - alphaVal: scalar used to scale the strict upper triangular ones
  %   - upperBand: optional truncation parameter (defaults to n-1)
  %   - className: numeric class for created arrays
  if numel(nSize) == 1
    mRows = nSize;
    nCols = nSize;
  else
    mRows = nSize(1);
    nCols = nSize(end);
  end
  
  % default upperBand covers full width of the (square) matrix
  if isempty(upperBand)
    upperBand = nCols - 1;
  end
  % default alphaVal is ones of requested class (if empty)
  if isempty(alphaVal)
    alphaVal = ones(className);
  end
  % Build T: identity plus alpha*strict-upper-triangular ones, then take lower part
  T = tril( eye(mRows,nCols,className) + alphaVal * triu(ones(mRows,nCols,className), 1), upperBand);
end
%=============================================================================
