%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if iswasm()
  skip_testsuite('bytecode engine is not available in the portable WebAssembly runtime.');
end
%=============================================================================
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
sourceDirectory = [directory, '/source'];
bytecodeDirectory = [directory, '/bytecode'];
mkdir(sourceDirectory);
mkdir(bytecodeDirectory);
write_lines([sourceDirectory, '/bc_perf_work.m'], {
  'function result = bc_perf_work(kind, count)'
  'result = 0;'
  'if kind == 1'
  'for k = 1:count; result = result + k * 0.25; end'
  'elseif kind == 2'
  'for k = 1:count; result = result + bc_perf_leaf(k); end'
  'else'
  'for k = 1:count; result = result + sin(k * 0.001); end'
  'end'
  'end'
  });
write_lines([sourceDirectory, '/bc_perf_leaf.m'], {
  'function result = bc_perf_leaf(value)'
  'result = value + 1;'
  'end'
  });
__bytecode_artifact__('compile', [sourceDirectory, '/bc_perf_work.m'], [bytecodeDirectory, '/bc_perf_work.nbc']);
__bytecode_artifact__('compile', [sourceDirectory, '/bc_perf_leaf.m'], [bytecodeDirectory, '/bc_perf_leaf.nbc']);
counts = [3000000, 500000, 500000];
samples = zeros(10, 2, 3);
cpuSamples = zeros(10, 2, 3);
firstCalls = zeros(10, 2);
expected = zeros(1, 3);
for repetition = 1:10
  order = [1, 2];
  if mod(repetition, 2) == 0
    order = [2, 1];
  end
  for variant = order
    if variant == 1
      activeDirectory = sourceDirectory;
    else
      activeDirectory = bytecodeDirectory;
    end
    addpath(activeDirectory);
    pathCleanup = onCleanup(@() rmpath(activeDirectory));
    clear bc_perf_work bc_perf_leaf;
    tic();
    bc_perf_work(2, 1);
    firstCalls(repetition, variant) = toc();
    for kind = 1:3
      bc_perf_work(kind, 2000);
      cpuStart = cputime();
      tic();
      value = bc_perf_work(kind, counts(kind));
      samples(repetition, variant, kind) = toc();
      cpuSamples(repetition, variant, kind) = cputime() - cpuStart;
      if repetition == 1 && variant == 1
        expected(kind) = value;
      end
      asserts.isequal(value, expected(kind));
    end
    clear bc_perf_work bc_perf_leaf pathCleanup;
  end
end
asserts.isequal(expected(1), counts(1) * (counts(1) + 1) / 8);
asserts.isequal(expected(2), counts(2) * (counts(2) + 3) / 2);
for kind = 1:3
  sourceMedian = median(samples(:, 1, kind));
  bytecodeMedian = median(samples(:, 2, kind));
  fprintf('BYTECODE_BENCH kind=%d source=%.9g bytecode=%.9g ratio=%.6g\n', kind, sourceMedian, bytecodeMedian, bytecodeMedian / sourceMedian);
  sourceCPU = median(cpuSamples(:, 1, kind));
  bytecodeCPU = median(cpuSamples(:, 2, kind));
  fprintf('BYTECODE_CPU kind=%d source=%.9g bytecode=%.9g ratio=%.6g\n', kind, sourceCPU, bytecodeCPU, bytecodeCPU / sourceCPU);
end
fprintf('BYTECODE_FIRST_CALL source=%.9g bytecode=%.9g\n', median(firstCalls(:, 1)), median(firstCalls(:, 2)));
fprintf('BYTECODE_BENCH_OK\n');
clear cleanup;

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  asserts.istrue(fid >= 0);
  for k = 1:numel(lines)
    fprintf(fid, '%s\n', lines{k});
  end
  fclose(fid);
end
