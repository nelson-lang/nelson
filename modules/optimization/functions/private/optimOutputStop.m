%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stop = optimOutputStop(outputFcn, x, optimValues, state)
  stop = false;
  if isempty(outputFcn)
    return
  end
  if iscell(outputFcn)
    for k = 1:numel(outputFcn)
      stop = stop || optimOutputStop(outputFcn{k}, x, optimValues, state);
    end
    return
  end
  if isstring(outputFcn)
    outputFcn = char(outputFcn);
  end
  if ischar(outputFcn)
    outputFcn = str2func(outputFcn);
  end
  if isa(outputFcn, 'function_handle')
    stop = logical(outputFcn(x, optimValues, state));
  end
end
%=============================================================================
