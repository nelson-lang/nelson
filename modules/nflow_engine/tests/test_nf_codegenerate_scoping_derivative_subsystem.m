%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 64-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% The flattener adds no derivative latency inside a subsystem.
[~, dd2] = nflow_scoping_derivative_diagrams();
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dd2, 'nfsd2_num_c', 'C', {'yd'}, {'nfsd2_y'}, 5e-6, 1e-9);
end
%=============================================================================
