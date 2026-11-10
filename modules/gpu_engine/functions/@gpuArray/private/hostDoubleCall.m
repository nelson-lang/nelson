%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = hostDoubleCall(name, varargin)
  % runs a host function that accepts double but not single, numeric results go back to the device
  hostArgs = cellfun(@toHostDouble, varargin, 'UniformOutput', false);
  result = feval(name, hostArgs{:});
  if (isnumeric(result) || islogical(result)) && ~issparse(result) && ~isa(result, 'int64') && ~isa(result, 'uint64')
    result = gpuArray(result);
  end
end
%=============================================================================
function x = toHostDouble(x)
  % device floats come back as double
  if isa(x, 'gpuArray')
    x = gather(x);
    if isa(x, 'single')
      x = double(x);
    end
  end
end
%=============================================================================
