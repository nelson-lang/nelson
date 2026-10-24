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
    activeAlpha = -ones(className);
  else
    activeAlpha = alphaVal;
  end
  T = triw(nSize, activeAlpha, [], className);
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

  if isempty(upperBand)
    activeUpperBand = nCols - 1;
  else
    activeUpperBand = upperBand;
  end
  if isempty(alphaVal)
    activeAlpha = ones(className);
  else
    activeAlpha = alphaVal;
  end
  T = eye(mRows, nCols, className);
  T(triu(true(mRows, nCols), 1)) = activeAlpha;
  T = tril(T, activeUpperBand);
end
%=============================================================================
