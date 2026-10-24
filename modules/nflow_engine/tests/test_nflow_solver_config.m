%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% B4: scripted solver configuration. set_param / get_param accept the model
% Solver and its tolerances (RelTol / AbsTol / MaxStep), stored in the model so
% they reach the engine (numbers for the tolerances).
%=============================================================================
bdclose('all');
new_system('cfg');
add_block('nflow/math/gain', 'cfg/G', 'gain', 1);

set_param('cfg', 'Solver', 'ode4');
asserts.isequal(get_param('cfg', 'Solver'), 'ode4');

set_param('cfg', 'RelTol', 1e-5);
asserts.isapprox(str2double(get_param('cfg', 'RelTol')), 1e-5, 0, 1e-15);

set_param('cfg', 'AbsTol', 1e-7);
asserts.isapprox(str2double(get_param('cfg', 'AbsTol')), 1e-7, 0, 1e-15);

set_param('cfg', 'MaxStep', 0.02);
asserts.isapprox(str2double(get_param('cfg', 'MaxStep')), 0.02, 0, 1e-15);

% Non-numeric tolerance is rejected.
asserts.checkerror('set_param(''cfg'', ''RelTol'', ''abc'')', ...
  _('Model parameter ''RelTol'' must be a number.'));

% Solver survives a subsequent valid update.
set_param('cfg', 'Solver', 'variableStiff');
asserts.isequal(get_param('cfg', 'Solver'), 'variableStiff');

bdclose('all');
%=============================================================================
