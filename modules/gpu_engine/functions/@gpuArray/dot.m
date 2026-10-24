%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = dot(a, b, dim)
  % dot(a, b) = sum(conj(a) .* b). Runs on the device by composing the gpuArray
  % overloads; the result stays device-resident.
  if nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin < 3
    sa = size(a);
    sb = size(b);
    aVec = (length(sa) == 2) && (min(sa) == 1);
    bVec = (length(sb) == 2) && (min(sb) == 1);
    if aVec && bVec
      % two vectors: orientation-independent scalar
      r = sum(conj(a(:)) .* b(:));
    else
      r = sum(conj(a) .* b);
    end
  else
    r = sum(conj(a) .* b, dim);
  end
end
%=============================================================================
