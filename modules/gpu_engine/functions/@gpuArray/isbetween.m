%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isbetween(A, lower, upper, varargin)
  % elements inside the interval, on the device; other forms run on the host
  ok = false;
  if nargin >= 3
    [tf, ok] = __gpuArray_between__(A, lower, upper, varargin{:});
  end
  if ~ok
    tf = __gpuArray_host__('isbetween', A, lower, upper, varargin{:});
  end
end
%=============================================================================
