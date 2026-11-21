%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Generated models have no error return from nflow_step. A singular matrix
% must therefore yield a detectable NaN instead of a plausible finite value.
hasC = __nflow_codegen_toolchain__('C');
hasRust = __nflow_codegen_toolchain__('rust');
skip_testsuite(~hasC && ~hasRust, 'C or Rust toolchain unavailable');
d = struct();
d.blocks = {struct('id','b','type','constant','inputs',0,'outputs',1,'params',struct('Value',[1 2 2 4])), ...
  struct('id','shape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2])), ...
  struct('id','p','type','mult','inputs',1,'outputs',1,'params',struct('Inputs','/','Multiplication','Matrix(*)')), ...
  struct('id','sel','type','selector','inputs',1,'outputs',1,'params',struct('Indices',1)), ...
  struct('id','y','type','labelSink','inputs',1,'outputs',1,'params',struct('GotoTag','y','isExternalPort',true))};
d.connections = {struct('from','b','to','shape'), struct('from','shape','to','p'), ...
  struct('from','p','to','sel'), struct('from','sel','to','y')};
d.sampleTime = 0.01; d.duration = 0.1; d.variables = struct();
for complexCase = 0:2
 if complexCase == 1
  d.blocks{end + 1} = struct('id','im','type','constant','inputs',0,'outputs',1,'params',struct('Value',[0 0 0 0]));
  d.blocks{end + 1} = struct('id','imshape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2]));
  d.blocks{end + 1} = struct('id','combine','type','realImagToComplex','inputs',2,'outputs',1,'params',struct());
  d.blocks{end + 1} = struct('id','split','type','complexToRealImag','inputs',1,'outputs',2,'params',struct());
  d.connections = {struct('from','b','to','shape'), struct('from','im','to','imshape'), ...
    struct('from','shape','to','combine','toIndex',0), struct('from','imshape','to','combine','toIndex',1), ...
    struct('from','combine','to','p'), struct('from','p','to','sel'), ...
    struct('from','sel','to','split'), struct('from','split','fromIndex',0,'to','y')};
 end
 if complexCase == 2
  d.blocks = {struct('id','b','type','constant','inputs',0,'outputs',1,'params',struct('Value',0)), ...
    struct('id','p','type','mult','inputs',1,'outputs',1,'params',struct('Inputs','/','Multiplication','Matrix(*)')), ...
    struct('id','y','type','labelSink','inputs',1,'outputs',1,'params',struct('GotoTag','y','isExternalPort',true))};
  d.connections = {struct('from','b','to','p'), struct('from','p','to','y')};
 end
 r = jsondecode(__nflow_simulate__(jsonencode(d)));
 assert_isequal(r.status, 'error');
 for target = {'C', 'rust'}
  if (strcmp(target{1}, 'C') && ~hasC) || (strcmp(target{1}, 'rust') && ~hasRust)
    continue;
  end
  stem = ['nflow_matrix_singular_', num2str(complexCase), '_', target{1}];
  model = [tempdir(), stem, '.nflow'];
  fid = fopen(model, 'wt'); fwrite(fid, jsonencode(d)); fclose(fid);
  nflow_codegenerate(model, tempdir(), true, target{1}, struct('build', true, 'buildConfig', 'Release'));
  if strcmp(target{1}, 'rust')
    exe = [tempdir(), stem, '/target/release/nflow_run'];
  elseif ispc()
    exe = [tempdir(), stem, '/build/Release/nflow_run'];
  else
    exe = [tempdir(), stem, '/build/nflow_run'];
  end
  if ispc(), exe = [exe, '.exe']; end
  csv = [tempdir(), stem, '.csv'];
  [status, output] = system(['"', exe, '" -t 0.1 -o "', csv, '"']);
  assert_isequal(status, 0, output);
  contents = lower(fileread(csv));
  assert_istrue(~isempty(strfind(contents, 'nan')));
 end
end
%=============================================================================
