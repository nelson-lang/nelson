%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% derivative parity: generated Rust matches the top-level runtime behavior.
[dd, ~] = nflow_scoping_derivative_diagrams();
if __nflow_codegen_toolchain__('rust')
  nflow_codegen_check_numeric(dd, 'nfsd_num_rust', 'rust', {'yd'}, {'nfsd_y'}, 5e-6, 1e-9);
end
%=============================================================================
