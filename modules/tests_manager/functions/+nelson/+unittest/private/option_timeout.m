%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function value = option_timeout(options)
  value = 0;
  if isfield(options, 'timeout') && isnumeric(options.timeout) ...
      && isscalar(options.timeout) && options.timeout > 0
    value = double(options.timeout);
  end
end
%=============================================================================
