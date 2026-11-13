%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = workersValue(workers)
  % Workers option: 'auto' becomes -1, a number is passed as is.
  if (ischar(workers) || (isstring(workers) && isscalar(workers))) && strcmpi(char(workers), 'auto')
    value = -1;
  elseif isnumeric(workers) && isscalar(workers)
    value = double(workers);
  else
    error('Nelson:io:zstd:invalidWorkers', 'Workers must be ''auto'' or an integer from 0 to 256.');
  end
end
%=============================================================================
