%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function args = odeAppendParameters(args, parameters)
  if isempty(parameters)
    return
  end
  if iscell(parameters)
    args = [args, parameters];
  else
    args = [args, {parameters}];
  end
end
%=============================================================================
