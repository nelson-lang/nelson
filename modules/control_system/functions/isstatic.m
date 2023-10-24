%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Convention used:
% Ts > 0: Discrete-time model.
% Ts = 0: Continuous-time model.
% Ts = -1: Discrete-time model with unspecified sampling time.
% Ts = -2: Static gain model.
function res = isstatic(sys)
  msg = sprintf(_('Ensure that the data type of the argument is correct in the function call to ''%s''.'), 'isstatic');
  error(msg);
end
%=============================================================================

