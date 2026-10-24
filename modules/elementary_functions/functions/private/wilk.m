%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
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

  if nSize == 3
    matrixA = cast(reshape([1e-10 0 0 .9 .9 0 -.4 -.4 1e-10], 3, 3), className);
    vectorB = cast([0 0 1]', className);
  elseif nSize == 4
    matrixA = wilkFour(className);
    vectorB = cast([0.6524 0.3127 0.4186 0.7853]', className);
  elseif nSize == 5
    hilbertMatrix = hilb(6, className);
    matrixA = 1.8144 * hilbertMatrix(1:5, 2:6);
    vectorB = [];
  elseif nSize == 21
    matrixA = wilkTwentyOne(className);
    vectorB = [];
  else
    error('Nelson:elementary_functions:wrongValueNSupportedValues', 'Wrong value for N. Supported values are 3, 4, 5, or 21.');
  end
end
%=============================================================================
function matrixA = wilkFour(className)
  columns = [0.9143e-4 0.8762 0.7943 0.8017 ...
             0 0.7156e-4 0.8143 0.6123 ...
             0 0 0.9504e-4 0.7165 ...
             0 0 0 0.7123e-4];
  matrixA = cast(reshape(columns, 4, 4), className);
end
%=============================================================================
function matrixA = wilkTwentyOne(className)
  nSize = 21;
  superDiagonal = diag(ones(nSize - 1, 1, className), 1);
  midpoint = (nSize - 1) / 2;
  diagonalValues = cast(abs(-midpoint:midpoint), className);
  matrixA = diag(diagonalValues) + superDiagonal + superDiagonal';
end
%=============================================================================
