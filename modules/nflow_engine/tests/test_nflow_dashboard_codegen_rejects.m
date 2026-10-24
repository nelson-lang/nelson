%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
composite = nflow_dashboard_model();
composite.blocks{5}.params.Binding.element = '(1)';
nflow_dashboard_assert_codegen_rejected(composite, 'C', 'dashboard_binding(control_ui)');
nflow_dashboard_assert_codegen_rejected(composite, 'rust', 'dashboard_binding(control_ui)');
structural = nflow_structural_dashboard_model();
nflow_dashboard_assert_codegen_rejected(structural, 'C', 'dashboard_binding(delay_control)');
nflow_dashboard_assert_codegen_rejected(structural, 'rust', 'dashboard_binding(delay_control)');
%=============================================================================
