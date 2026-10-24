%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function unsetenv(env_name)
  narginchk(1, 1);
  nargoutchk(0, 0);
  if ~(ischar(env_name) || (isStringScalar(env_name)))
    error('Nelson:unsetenv:invalidInputType', 'Wrong type for argument #%d: %s expected.', 1, 'string');
  end
  setenv(char(env_name));
end
%=============================================================================
