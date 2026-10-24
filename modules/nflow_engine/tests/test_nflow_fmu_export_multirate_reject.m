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
% A per-block SampleTime slower than the base step sub-rates in the simulator,
% but the exported FMU emits fixed-step code with no lowering for
% it. NFlow.exportfmu must reject such a multi-rate model explicitly rather than
% produce an FMU that does not match the simulation (same policy as the C/Rust
% code generator).
%=============================================================================
work = tempname();
mkdir(work);
cleaner = onCleanup(@() rmdir(work, 's')); %#ok<NASGU>
d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2, 'SampleTime', 0.2)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'u', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
f = [work, filesep, 'M.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
%=============================================================================
msg = '';
try
  NFlow.exportfmu(f, [work, filesep, 'out']);
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'multi-rate')));
assert_istrue(~isempty(strfind(msg, 'block type')));
assert_istrue(~isempty(strfind(msg, 'g')));
%=============================================================================
