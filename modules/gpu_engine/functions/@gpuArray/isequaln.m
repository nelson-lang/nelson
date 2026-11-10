%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isequaln(varargin)
  % numeric arrays are compared on the device with NaN equal to NaN
  tf = compareOnDevice(varargin, true);
  if isempty(tf)
    tf = __gpuArray_host__('isequaln', varargin{:});
  end
end
%=============================================================================
