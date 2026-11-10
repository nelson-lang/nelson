%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, a, b] = __gpuArray_set_operands__(a, b, allowScalar)
  % non-empty real numeric arrays of one class that the device set functions take, as device
  % arrays; scalars only when allowScalar (their empty results follow other shape rules)
  tf = isnumeric(a) && isnumeric(b) && isreal(a) && isreal(b) && ~isempty(a) && ~isempty(b) ...
    && ~issparse(a) && ~issparse(b) && ismatrix(a) && ismatrix(b) ...
    && any(strcmp(storedType(a), {'single'})) && strcmp(storedType(a), storedType(b)) ...
    && (allowScalar || (~isscalar(a) && ~isscalar(b)));
  if tf
    if ~isgpuarray(a)
      a = gpuArray(a);
    end
    if ~isgpuarray(b)
      b = gpuArray(b);
    end
  end
end
%=============================================================================
function t = storedType(x)
  % the device keeps double values as single
  t = underlyingType(x);
  if strcmp(t, 'double')
    t = 'single';
  end
end
%=============================================================================
