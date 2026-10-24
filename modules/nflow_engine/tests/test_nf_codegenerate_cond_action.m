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
% <--WEIGHT 64-->
% <--TIMEOUT 600-->
%=============================================================================
% Action gate: level semantics identical to enable in the runtime (active
% while the control is > 0), so the same lowering applies verbatim.
%=============================================================================
da = nflow_cond_enable_base_diagram('nfca_y');
da.blocks{4}.params.controlPort = struct('kind', 'action');
da.blocks{4}.params.subsystem.controlPort = struct('kind', 'action');
r4 = jsondecode(__nflow_simulate__(jsonencode(da)));
assert_isequal(r4.status, 'done');
ya = evalin('base', 'nfca_y');
assert_istrue(abs(ya(end) - ya(7)) < 1e-12); % held once disabled, like enable
evalin('base', 'clear(''nfca_y'')');
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(da, 'nfca_num', 'C', {'y'}, {'nfca_y'}, 5e-6, 1e-9);
end
%=============================================================================
