%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
hasC = __nflow_codegen_toolchain__('C');
hasRust = __nflow_codegen_toolchain__('rust');
skip_testsuite(~hasC && ~hasRust, 'C or Rust toolchain unavailable');
d = struct();
d.blocks = {struct('id','a','type','constant','inputs',0,'outputs',1,'params',struct('Value',[1 3 2 4])), ...
  struct('id','ashape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2])), ...
  struct('id','b','type','constant','inputs',0,'outputs',1,'params',struct('Value',[5 7 6 8])), ...
  struct('id','bshape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2])), ...
  struct('id','p','type','mult','inputs',2,'outputs',1,'params',struct('Inputs','*/','Multiplication','Matrix(*)')), ...
  struct('id','sel','type','selector','inputs',1,'outputs',1,'params',struct('Indices',1)), ...
  struct('id','y','type','labelSink','inputs',1,'outputs',1,'params',struct('GotoTag','y','isExternalPort',true)), ...
  struct('id','tw','type','toWorkspace','inputs',1,'outputs',0,'params',struct('VariableName','matrix_product_ref','SaveFormat','Array','Decimation',1,'MaxDataPoints','inf'))};
d.connections = {struct('from','a','to','ashape'), struct('from','b','to','bshape'), ...
  struct('from','ashape','to','p','toIndex',0), struct('from','bshape','to','p','toIndex',1), ...
  struct('from','p','to','sel'), struct('from','sel','to','y'), struct('from','sel','to','tw')};
d.sampleTime = 0.01; d.duration = 0.1; d.variables = struct();
if hasC
  nflow_codegen_check_numeric(d, 'nflow_matrix_product_c', 'C', {'y'}, {'matrix_product_ref'}, 1e-9, 1e-9);
end
if hasRust
  nflow_codegen_check_numeric(d, 'nflow_matrix_product_rust', 'rust', {'y'}, {'matrix_product_ref'}, 1e-9, 1e-9);
end
% Integer products saturate each multiplication and addition in both targets.
d.blocks{1}.params.Value = [100 0 100 0];
d.blocks{1}.params.OutDataType = 'int8';
d.blocks{3}.params.Value = [2 -2 0 0];
d.blocks{3}.params.OutDataType = 'int8';
d.blocks{5}.params.Inputs = '**';
if hasC
  nflow_codegen_check_numeric(d, 'nflow_matrix_integer_c', 'C', {'y'}, {'matrix_product_ref'}, 0, 0);
end
if hasRust
  nflow_codegen_check_numeric(d, 'nflow_matrix_integer_rust', 'rust', {'y'}, {'matrix_product_ref'}, 0, 0);
end
% Single-precision division follows the same intermediate rounding.
d.blocks{1}.params.Value = [1 3 2 4];
d.blocks{1}.params.OutDataType = 'single';
d.blocks{3}.params.Value = [5 7 6 8];
d.blocks{3}.params.OutDataType = 'single';
d.blocks{5}.params.Inputs = '*/';
if hasC
  nflow_codegen_check_numeric(d, 'nflow_matrix_single_c', 'C', {'y'}, {'matrix_product_ref'}, 1e-5, 1e-5);
end
if hasRust
  nflow_codegen_check_numeric(d, 'nflow_matrix_single_rust', 'rust', {'y'}, {'matrix_product_ref'}, 1e-5, 1e-5);
end
% Integer inputs can be converted to double before matrix division.
d.blocks{1}.params.OutDataType = 'int8';
d.blocks{3}.params.OutDataType = 'int8';
d.blocks{end + 1} = struct('id','aconv','type','convert','inputs',1,'outputs',1,'params',struct('OutDataType','double'));
d.blocks{end + 1} = struct('id','bconv','type','convert','inputs',1,'outputs',1,'params',struct('OutDataType','double'));
d.connections = {struct('from','a','to','ashape'), struct('from','b','to','bshape'), ...
  struct('from','ashape','to','aconv'), struct('from','bshape','to','bconv'), ...
  struct('from','aconv','to','p','toIndex',0), struct('from','bconv','to','p','toIndex',1), ...
  struct('from','p','to','sel'), struct('from','sel','to','y'), struct('from','sel','to','tw')};
if hasC
  nflow_codegen_check_numeric(d, 'nflow_matrix_converted_c', 'C', {'y'}, {'matrix_product_ref'}, 1e-9, 1e-9);
end
if hasRust
  nflow_codegen_check_numeric(d, 'nflow_matrix_converted_rust', 'rust', {'y'}, {'matrix_product_ref'}, 1e-9, 1e-9);
end
%=============================================================================
