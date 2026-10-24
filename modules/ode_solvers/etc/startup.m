%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath(modulepath('ode_solvers', 'functions'), '-begin', '-frozen');
builtinPath = modulepath('ode_solvers', 'builtin');
try
  addgateway(builtinPath, 'ode_solvers');
  % Force the builtin library to load now so its variable-step CVODES backend
  % registers with the NFlow solver-backend registry at startup. The gateway DLL
  % is otherwise loaded lazily (on first ode builtin call), which would leave
  % NFlow's "variableStiff"/"variableNonstiff" solvers unavailable until then.
  __ode_sundials_available__();
catch
end
%=============================================================================
