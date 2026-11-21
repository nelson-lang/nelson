%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Generated product with division must match the simulator.
hasC = __nflow_codegen_toolchain__('C');
hasRust = __nflow_codegen_toolchain__('rust');
skip_testsuite(~hasC && ~hasRust, 'C or Rust toolchain unavailable');
d = struct();
d.blocks = {struct('id','a','type','constant','inputs',0,'outputs',1,'params',struct('Value',12)), ...
  struct('id','b','type','constant','inputs',0,'outputs',1,'params',struct('Value',2)), ...
  struct('id','c','type','constant','inputs',0,'outputs',1,'params',struct('Value',3)), ...
  struct('id','p','type','mult','inputs',3,'outputs',1,'params',struct('Inputs','*/*')), ...
  struct('id','y','type','labelSink','inputs',1,'outputs',1,'params',struct('GotoTag','y','isExternalPort',true)), ...
  struct('id','tw','type','toWorkspace','inputs',1,'outputs',0,'params',struct('VariableName','product_ref','SaveFormat','Array','Decimation',1,'MaxDataPoints','inf'))};
d.connections = {struct('from','a','to','p','toIndex',0), struct('from','b','to','p','toIndex',1), ...
  struct('from','c','to','p','toIndex',2), struct('from','p','to','y'), struct('from','p','to','tw')};
d.sampleTime = 0.01; d.duration = 0.1; d.variables = struct();
if hasC
  nflow_codegen_check_numeric(d, 'nflow_product_operations_c', 'C', {'y'}, {'product_ref'}, 1e-10, 1e-10);
end
if hasRust
  nflow_codegen_check_numeric(d, 'nflow_product_operations_rust', 'rust', {'y'}, {'product_ref'}, 1e-10, 1e-10);
end
%=============================================================================
