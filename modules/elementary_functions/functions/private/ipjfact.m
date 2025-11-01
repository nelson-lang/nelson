%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [A, detA] = ipjfact(nSize, kind, className)
  if nargin < 2 || isempty(kind)
    kind = 0;
  end
  if nargin < 3 || isempty(className)
    className = 'double';
  end
  
  % build vectors for Hankel via helper
  [cVec, tailVec] = build_hankel_vectors(nSize);
  
  % main Hankel matrix
  A = cast(hankel(cVec, tailVec), className);
  
  % invert entries when requested (kind == 1)
  if kind == 1
    A = ones(nSize, className) ./ A;
  end
  
  % compute determinant only if requested
  if nargout > 1
    detA = compute_determinant(nSize, kind);
  end
end

% Local helper: build the first and last column vectors for the Hankel matrix
function [cVec, tailVec] = build_hankel_vectors(nSize)
  cVec = cumprod(2:nSize+1);
  tailVec = cumprod(nSize+1:2*nSize) * cVec(nSize-1);
end

% Local helper: compute determinant when requested (keeps original logic)
function determinantVar = compute_determinant(nSize, kind)
  determinantVar = 1;
  if kind == 0
    for idx = 1:nSize-1
      determinantVar = determinantVar * prod(1:idx+1) * prod(1:nSize-idx);
    end
    determinantVar = determinantVar * prod(1:nSize+1);
  else
    for idx = 0:nSize-1
      determinantVar = determinantVar * prod(1:idx) / prod(1:nSize+1+idx);
    end
    % flip sign depending on parity of n*(n-1)/2
    if (rem(nSize*(nSize-1)/2, 2) ~= 0)
      determinantVar = -determinantVar;
    end
  end
end
