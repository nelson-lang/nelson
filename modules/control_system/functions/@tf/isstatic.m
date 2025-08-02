%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
  res = isequal(sys.Ts, -2);
end
%=============================================================================

