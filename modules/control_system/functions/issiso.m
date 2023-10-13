%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = issiso(sys)
  if ~islti(sys)
    msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'issiso');
    error(msg);
  end
  
  [p, m] = size(sys);
  res = (p == 1) && (m == 1);
end
%=============================================================================
