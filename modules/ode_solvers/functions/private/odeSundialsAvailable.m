%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = odeSundialsAvailable()
  tf = __ode_sundials_available__();
  if ~tf
    return
  end
  mode = upper(getenv('NELSON_SUNDIALS_RUNTIME'));
  tf = ~(strcmp(mode, 'OFF') || strcmp(mode, '0') || strcmp(mode, 'FALSE'));
end
%=============================================================================
