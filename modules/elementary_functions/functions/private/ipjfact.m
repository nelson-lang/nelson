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
    activeKind = 0;
  else
    activeKind = kind;
  end
  if nargin < 3 || isempty(className)
    outputClass = 'double';
  else
    outputClass = className;
  end

  factorialIndex = (1:nSize).' + (1:nSize);
  A = cast(factorial(factorialIndex), outputClass);
  if activeKind == 1
    A = ones(nSize, outputClass) ./ A;
  end

  if nargout > 1
    detA = ipjfactDeterminant(nSize, activeKind);
  end
end
%=============================================================================
function determinantVar = ipjfactDeterminant(nSize, activeKind)
  determinantVar = 1;
  if activeKind == 0
    for idx = 1:nSize - 1
      determinantVar = determinantVar * prod(1:idx + 1) * prod(1:nSize - idx);
    end
    determinantVar = determinantVar * prod(1:nSize + 1);
  else
    for idx = 0:nSize - 1
      determinantVar = determinantVar * prod(1:idx) / prod(1:nSize + 1 + idx);
    end
    if rem(nSize * (nSize - 1) / 2, 2) ~= 0
      determinantVar = -determinantVar;
    end
  end
end
%=============================================================================
