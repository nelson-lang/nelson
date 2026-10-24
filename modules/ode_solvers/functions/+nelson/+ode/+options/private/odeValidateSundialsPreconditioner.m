%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = odeValidateSundialsPreconditioner(value)
  value = odeValidateSundialsChoice(value, 'Preconditioner', ...
    {'auto', 'none', 'jacobi', 'banded', 'ilu0'});
end
%=============================================================================
