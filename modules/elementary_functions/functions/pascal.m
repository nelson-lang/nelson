%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0014,NLS0015,NLS0026>
function varargout = pascal(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  nSize = varargin{1};
  kind = 0;
  className = 'double';
  if nargin >= 2 && ~isempty(varargin{2})
    kind = varargin{2};
  end
  if nargin >= 3 && ~isempty(varargin{3})
    className = varargin{3};
  end
  if nargin == 2 && (ischar(kind) || (isstring(kind) && isscalar(kind)))
    className = kind;
    kind = 0;
  end
  if isstring(className) && isscalar(className)
    className = char(className);
  end
  mustBePositive(nSize, 1);
  mustBeInteger(nSize, 1);
  PascalMatrix = signedPascalTriangle(double(nSize), className);
  if kind == 0
    PascalMatrix = PascalMatrix * PascalMatrix';
  elseif kind == 2
    PascalMatrix = rot90(PascalMatrix, 3);
    if rem(nSize, 2) == 0
      PascalMatrix = -PascalMatrix;
    end
  elseif kind ~= 1
    error('Nelson:elementary_functions:wrongValue2Argument0', 'Wrong value for #%d argument. 0, 1, or 2 expected.', 2);
  end
  varargout{1} = cast(PascalMatrix, className);
end
%=============================================================================
function L = signedPascalTriangle(n, className)
  L = zeros(n, n, className);
  for row = 1:n
    for col = 1:row
      coefficient = nchoosek(row - 1, col - 1);
      if rem(col, 2) == 0
        coefficient = -coefficient;
      end
      L(row, col) = coefficient;
    end
  end
end
%=============================================================================
