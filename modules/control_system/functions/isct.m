%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: MIT OR LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Convention used:
% Ts > 0: Discrete-time model.
% Ts = 0: Continuous-time model.
% Ts = -1: Discrete-time model with unspecified sampling time.
% Ts = -2: Static gain model.
function res = isct(sys)
    if ~islti(sys) 
        msg = sprintf(_('Ensure that the data type of the argument is correct in the function call to ''%s''.'), 'isct');
        error(msg);
    end
    res  = (sys.Ts == 0) || (sys.Ts == -2);
end
%=============================================================================
