%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = allbetween(A, lower, upper, varargin)
  % true when every element is inside the interval, on the device; other forms run on the host
  ok = false;
  if nargin >= 3
    [inside, ok] = __gpuArray_between__(A, lower, upper, varargin{:});
  end
  if ok
    tf = all(inside(:));
  else
    tf = __gpuArray_host__('allbetween', A, lower, upper, varargin{:});
  end
end
%=============================================================================
