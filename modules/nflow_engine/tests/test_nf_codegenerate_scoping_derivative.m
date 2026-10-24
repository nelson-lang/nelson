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
%=============================================================================
% derivative parity: OUTPUT recomputes the backward difference from the current
% input at each sample (no one-sample latch), and the generated step mirrors it.
% Verified top level in generated C.
[dd, ~] = nflow_scoping_derivative_diagrams();
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dd, 'nfsd_num_c', 'C', {'yd'}, {'nfsd_y'}, 5e-6, 1e-9);
end
%=============================================================================
