%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ENGLISH IMPOSED-->
%=============================================================================
d = nflow_dashboard_model();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_istrue(isfield(r, 'dashboardUpdates'));
assert_isequal(numel(r.dashboardUpdates), 2);
assert_isequal(r.dashboardUpdates(1).id, 'control_ui');
assert_istrue(r.dashboardUpdates(1).connected);
assert_istrue(r.dashboardUpdates(1).valid);
assert_isapprox(r.dashboardUpdates(1).value, 3, 0, 1e-12);
assert_isequal(r.dashboardUpdates(2).id, 'display_ui');
assert_istrue(r.dashboardUpdates(2).connected);
assert_istrue(r.dashboardUpdates(2).valid);
assert_isapprox(r.dashboardUpdates(2).value, 6, 0, 1e-12);
assert_isequal(r.dashboardUpdates(2).valueType, 'double');
%=============================================================================
invalid = d;
invalid.blocks{6}.params.Binding.outputPortIndex = 2;
invalidResult = jsondecode(__nflow_simulate__(jsonencode(invalid)));
assert_isequal(invalidResult.status, 'error');
assert_istrue(~isempty(strfind(invalidResult.reason, 'dashboard_binding(display_ui)')));
assert_isequal(invalidResult.errorBlocks{1}, 'display_ui');
%=============================================================================
