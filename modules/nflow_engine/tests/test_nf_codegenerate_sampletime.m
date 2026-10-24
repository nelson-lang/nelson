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
%=============================================================================
% nflow_codegenerate must honour the model's own sampleTime. Previously the
% builtin ignored the .nflow sampleTime field and emitted the default 0.01 s
% step, producing numerically wrong fixed-step code for any other dt.
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'S', 'type', 'step', 'inputs', 0, 'outputs', 1, 'params', struct('Time', 1)), ...
  struct('id', 'G', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('gain', 2)), ...
  struct('id', 'T', 'type', 'terminator', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {...
  struct('from', 'S', 'to', 'G', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'G', 'to', 'T', 'fromIndex', 0, 'toIndex', 0)};
d.duration = 1.0;
d.variables = struct();

function dt_line = generate_and_read_dt(d, stem)
  f = [tempdir(), stem, '.nflow'];
  fid = fopen(f, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  nflow_codegenerate(f, tempdir(), false);
  src = fileread([tempdir(), stem, filesep, 'model.c']);
  lines = strsplit(src, char(10));
  dt_line = '';
  for i = 1:numel(lines)
    if ~isempty(strfind(lines{i}, 'dt ='))
      dt_line = strtrim(lines{i});
      break;
    end
  end
end

% A model at dt = 0.001 must generate dt = 0.001 (not the old default 0.01).
d.sampleTime = 0.001;
line1 = generate_and_read_dt(d, 'st_model_a');
asserts.istrue(~isempty(strfind(line1, '0.001')));
asserts.isequal(numel(strfind(line1, '0.01;')), 0);

% A different step propagates too.
d.sampleTime = 0.02;
line2 = generate_and_read_dt(d, 'st_model_b');
asserts.istrue(~isempty(strfind(line2, '0.02')));
%=============================================================================
