%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [matrixA, vectorB] = wilk(varargin)
  % wilk  Return a Wilkinson-like test matrix (several preset sizes).
  %   [A,B] = wilk(N, CLASSNAME)
  %   Supported N values: 3, 4, 5, 21.
  %   CLASSNAME (optional) selects numeric class for returned arrays.
  if nargin < 2 || isempty(varargin{2})
    className = 'double';
  else 
    className = varargin{2};
  end
  nSize = varargin{1};
  
  % Case N = 3: explicit small ill-conditioned example with RHS
  if nSize == 3
    matrixA = [ 1e-10   .9  -.4;
    0     .9  -.4;
    0      0  1e-10];
    vectorB = [0 0 1]';
    matrixA = cast(matrixA, className);
    vectorB = cast(vectorB, className);
    % Case N = 4: another small example with RHS
  elseif nSize == 4
    matrixA = [0.9143e-4  0          0          0;
    0.8762     0.7156e-4  0          0;
    0.7943     0.8143     0.9504e-4  0;
    0.8017     0.6123     0.7165     0.7123e-4];
    vectorB = [0.6524     0.3127     0.4186     0.7853]';
    matrixA = cast(matrixA, className);
    vectorB = cast(vectorB, className);
    % Case N = 5: build from a 6x6 Hilbert
  elseif nSize == 5
    % construct 6x6 Hilbert of requested class and extract submatrix
    matrixA = hilb(6, className);
    matrixA = matrixA(1:5, 2:6) * 1.8144;
    % no RHS returned in original implementation for N=5
    vectorB = [];
    % Case N = 21: structured tridiagonal-like matrix
  elseif nSize == 21
    % superdiagonal of ones (in requested class)
    superDiag = diag(ones(nSize-1,1,className), 1);
    % diagonal with symmetric absolute offsets centered at zero
    m = (nSize - 1) / 2;
    centerOffsets = cast(abs(-m:m), className);
    matrixA = diag(centerOffsets) + superDiag + superDiag';
    vectorB = [];
  else
    error(_('Wrong value for N. Supported values are 3, 4, 5, or 21.'));
  end
end
%=============================================================================
