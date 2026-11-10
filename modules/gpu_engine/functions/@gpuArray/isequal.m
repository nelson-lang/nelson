%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isequal(varargin)
  % numeric arrays are compared on the device, only the answer is gathered
  tf = compareOnDevice(varargin, false);
  if isempty(tf)
    tf = __gpuArray_host__('isequal', varargin{:});
  end
end
%=============================================================================
