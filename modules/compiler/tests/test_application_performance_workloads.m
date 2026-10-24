%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = fullfile(nelsonroot(), 'modules', 'main', 'tests', 'fixtures', 'nelsonc_performance');
addpath(directory);
cleanup = onCleanup(@() rmpath(directory));
asserts.isequal(nc_perf_work('scalar', 5), 3.75);
asserts.isequal(nc_perf_work('crossfile', 5), 20);
asserts.isapprox(nc_perf_work('builtin', 5), sum(sin((1:5) * 0.001)), 1e-14);
values = reshape(mod(1:65536, 31) / 31, 256, 256);
product = values * values.';
expected = 2 * (product(1, 1) + product(256, 256));
asserts.isapprox(nc_perf_work('matrix', 2), expected, 1e-12);
values = reshape(mod(1:1048576, 31) / 31, 1024, 1024);
product = values * values.';
asserts.isapprox(nc_perf_work('matrixwide', 1), product(1, 1) + product(1024, 1024), 1e-12);
previousThreads = maxNumCompThreads();
output = evalc('nc_perf_entry(''scalar'', ''5'', ''1'', ''2'');');
offset = strfind(output, 'NELSONC_PERF=');
record = jsondecode(strtrim(output(offset + length('NELSONC_PERF='):end)));
asserts.isequal(record.threads, 2);
asserts.isequal(record.result, 3.75);
asserts.isequal(maxNumCompThreads(), previousThreads);
output = evalc('nc_perf_entry(''scalar'', ''5'', ''1'');');
offset = strfind(output, 'NELSONC_PERF=');
record = jsondecode(strtrim(output(offset + length('NELSONC_PERF='):end)));
asserts.isequal(record.threads, 1);
asserts.isequal(maxNumCompThreads(), previousThreads);
for invalid = {'0', '1.5', '65', 'NaN', 'Inf'}
  identifier = '';
  try
    nc_perf_entry('scalar', '5', '1', invalid{1});
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, 'Nelson:nelsonc:benchmarkThreads');
  asserts.isequal(maxNumCompThreads(), previousThreads);
end
failed = false;
try
  nc_perf_entry('unknown', '5', '1', '2');
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(maxNumCompThreads(), previousThreads);
plan = jsondecode(__nelsonc_plan__('link', fullfile(directory, 'nc_perf_entry.m')));
asserts.istrue(plan.complete);
asserts.isfalse(plan.dynamicFallback);
asserts.isequal(numel(plan.files), 3);
clear nc_perf_entry nc_perf_work nc_perf_leaf cleanup;
