%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TF = ismissing(A, varargin)
  % missing entries on the device: NaN for floating point data, none for integers and logicals;
  % custom indicators run on the host
  if nargin == 1 && isfloat(A)
    TF = isnan(A);
  elseif nargin == 1 && (isinteger(A) || islogical(A))
    TF = isnan(single(A)) & false;
  else
    TF = __gpuArray_host__('ismissing', A, varargin{:});
  end
end
%=============================================================================
