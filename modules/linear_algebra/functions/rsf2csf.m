%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rsf2csf(varargin)
  narginchk(2, 2);
  nargoutchk(0, 2);
  U = varargin{1};
  T = varargin{2};
  order = size(T, 2);
  for blockEnd = order:-1:2
    subDiagonal = blockEnd - 1;
    if T(blockEnd, subDiagonal) ~= 0
      block = subDiagonal:blockEnd;
      rotation = schurBlockRotation(T(block, block), T(blockEnd, blockEnd), ...
        T(blockEnd, subDiagonal));
      T(block, subDiagonal:order) = rotation * T(block, subDiagonal:order);
      T(1:blockEnd, block) = T(1:blockEnd, block) * rotation';
      U(:, block) = U(:, block) * rotation';
      T(blockEnd, subDiagonal) = 0;
    end
  end
  varargout{1} = U;
  if (nargout == 2)
    varargout{2} = T;
  end
end
%=============================================================================
function rotation = schurBlockRotation(block, diagonalValue, subDiagonalValue)
  delta = eig(block) - diagonalValue;
  scale = hypot(delta(1), subDiagonalValue);
  cosineValue = delta(1) / scale;
  sineValue = subDiagonalValue / scale;
  rotation = [cosineValue', sineValue; -sineValue, cosineValue];
end
%=============================================================================
