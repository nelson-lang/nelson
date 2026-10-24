%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset();
fields = fieldnames(options);
ref = {'AbsTol'; 'BDF'; 'Events'; 'InitialStep'; 'Jacobian'; 'JConstant'; ...
  'JPattern'; 'LinearSolver'; 'Mass'; 'MassSingular'; 'MaxOrder'; 'MaxStep'; 'MinStep'; ...
  'NonNegative'; 'NormControl'; 'OutputFcn'; 'OutputSel'; 'Refine'; ...
  'RelTol'; 'Stats'; 'Preconditioner'; 'Vectorized'; 'MStateDependence'; ...
  'MvPattern'; 'InitialSlope'};
asserts.isequal(fields, ref);
%=============================================================================
options = odeset('RelTol', 1e-4, 'AbsTol', 1e-8);
asserts.isequal(odeget(options, 'RelTol'), 1e-4);
asserts.isequal(odeget(options, 'AbsTol'), 1e-8);
asserts.isequal(odeget(options, 'MaxStep', 12), 12);
%=============================================================================
options2 = odeset(options, 'MaxStep', 0.25);
asserts.isequal(odeget(options2, 'RelTol'), 1e-4);
asserts.isequal(odeget(options2, 'MaxStep'), 0.25);
%=============================================================================
