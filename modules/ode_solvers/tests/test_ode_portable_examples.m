%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rootPath = modulepath('ode_solvers', 'root');
run([rootPath, '/examples/ode_object_event_interpolation_example.m']);
run([rootPath, '/examples/ode_fully_implicit_consistent_example.m']);
run([rootPath, '/examples/ode_delay_sensitivity_example.m']);
ddeBvpAddedFeaturesPlotEnabled = false;
ddeBvpAddedFeaturesPlotVisible = 'off';
run([rootPath, '/examples/dde_bvp_added_features_example.m']);
odeSundialsSparsePlotEnabled = false;
odeSundialsSparsePlotVisible = 'off';
run([rootPath, '/examples/ode_sundials_sparse_preconditioner_example.m']);
clear time
%=============================================================================
