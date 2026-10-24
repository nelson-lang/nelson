%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = optimResolveMaxFunEvals(opts, numberOfVariables)
  value = opts.MaxFunEvals;
  if isempty(value) || ischar(value) || isstring(value)
    value = 100 * numberOfVariables;
  else
    value = double(value);
  end
end
%=============================================================================
