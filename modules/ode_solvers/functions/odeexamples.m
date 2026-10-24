%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function odeexamples(varargin)
  % ODEEXAMPLES Open ODE solver examples.
  %
  %   odeexamples()
  %   Opens the ODE workflow help page. Complete runnable scripts are also
  %   available in the ode_solvers/examples directory for events,
  %   interpolation, fully implicit equations, DDE/BVP workflows, delay
  %   sensitivities, and optional SUNDIALS sparse preconditioning.
  %
  %   Example:
  %     rootPath = modulepath('ode_solvers', 'root');
  %     run([rootPath, '/examples/dde_bvp_added_features_example.m'])
  %
  %   See also ode, ode45, dde23, bvp4c, ode_workflows.
  if nargin > 0
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  try
    doc('ode_workflows');
  catch
    disp(_('ODE examples are available from the ODE workflow help page.'));
    disp('help ode_workflows');
  end
end
%=============================================================================
