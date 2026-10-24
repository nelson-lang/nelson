%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--ENGLISH IMPOSED-->
%=============================================================================
codegenModel = nflow_nested_dashboard_model();
csrc = nflow_dashboard_generate(codegenModel, 'dashboard_codegen_c', 'C');
assert_istrue(~isempty(strfind(csrc, 'double dashboard_control_ui;')));
assert_istrue(~isempty(strfind(csrc, 'nflow_set_dashboard_control_ui')));
assert_istrue(~isempty(strfind(csrc, 'out_sub_gain = out_source * s->dashboard_control_ui;')));
assert_isequal(isempty(strfind(csrc, 'display_ui')), true);
%=============================================================================
rsrc = nflow_dashboard_generate(codegenModel, 'dashboard_codegen_rust', 'rust');
assert_istrue(~isempty(strfind(rsrc, 'pub dashboard_control_ui: f64')));
assert_istrue(~isempty(strfind(rsrc, 'pub fn set_dashboard_control_ui')));
assert_istrue(~isempty(strfind(rsrc, 'out_sub_gain = s.dashboard_control_ui * out_source;')));
assert_isequal(isempty(strfind(rsrc, 'display_ui')), true);
%=============================================================================
