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
  skip_testsuite('bytecode artifact engine is not available in the portable WebAssembly runtime.');
end
%=============================================================================
directory = tempname();
mkdir(directory);
source = [directory, '/artifact_probe.m'];
destination = [directory, '/artifact_probe.nbc'];
lines = {
  'function [value, callback] = artifact_probe(x)'
  '  arguments'
  '    x (1, 1) double'
  '  end'
  '  value = local_twice(x);'
  '  for k = 1:20'
  '    value = value + k * x;'
  '  end'
  '  callback = @increment;'
  '  function result = increment()'
  '    x = x + 1;'
  '    result = x;'
  '  end'
  'end'
  'function result = local_twice(value)'
  '  result = 2 * value;'
  'end'
};
write_lines(source, lines);
sourceContents = fileread(source);
__bytecode_artifact__('compile', source, destination);
info = __bytecode_artifact__('inspect', destination);
asserts.istrue(contains(info, 'NBC format='));
asserts.istrue(contains(info, 'functions=3'));
asserts.istrue(contains(info, 'BytecodeChunk artifact_probe'));
asserts.istrue(contains(info, 'BytecodeChunk increment'));
asserts.istrue(contains(info, 'BytecodeChunk local_twice'));
asserts.istrue(contains(info, 'SCALAR_EXPR'));
asserts.istrue(contains(info, 'nested=1'));
original = read_bytes(destination);
duplicate = [directory, '/duplicate.nbc'];
__bytecode_artifact__('compile', source, duplicate);
asserts.isequal(read_bytes(duplicate), original);
assert_rejected(@() __bytecode_artifact__('compile', source, destination));
asserts.isequal(read_bytes(destination), original);
assert_rejected(@() __bytecode_artifact__('compile', source, source));
asserts.isequal(fileread(source), sourceContents);
addpath(directory);
asserts.istrue(endsWith(which('artifact_probe'), '.m'));
[baseline, sourceCallback] = artifact_probe(3);
asserts.isequal(baseline, 636);
clear artifact_probe;
delete(source);
rehash();
asserts.istrue(endsWith(which('artifact_probe'), '.nbc'));
asserts.isequal(nargin('artifact_probe'), 1);
asserts.isequal(nargout('artifact_probe'), 2);
[value, callback] = artifact_probe(3);
asserts.isequal(value, baseline);
asserts.isequal(callback(), 4);
asserts.isequal(sourceCallback(), 4);
clear artifact_probe;
[value, reloadedCallback] = artifact_probe(10);
asserts.isequal(value, 2120);
asserts.isequal(callback(), 5);
asserts.isequal(reloadedCallback(), 11);
rmpath(directory);
asserts.isequal(callback(), 6);
clear callback sourceCallback reloadedCallback artifact_probe;

corrupt = [directory, '/corrupt.nbc'];
lengths = [0, 1, 7, 8, 12, numel(original) - 1];
for k = 1:numel(lengths)
  write_bytes(corrupt, original(1:lengths(k)));
  assert_rejected(@() __bytecode_artifact__('inspect', corrupt));
end
offsets = unique(round(linspace(1, numel(original), 24)));
for k = 1:numel(offsets)
  bytes = original;
  bytes(offsets(k)) = bitxor(bytes(offsets(k)), uint8(1));
  write_bytes(corrupt, bytes);
  assert_rejected(@() __bytecode_artifact__('inspect', corrupt));
end
write_bytes(corrupt, [original; uint8(0)]);
assert_rejected(@() __bytecode_artifact__('inspect', corrupt));
script = [directory, '/artifact_script.m'];
write_lines(script, {
  'error(''Compilation must not execute this script.'');'
  'values = {true, "alpha", single(2), [], int64(3)};'
  'function x = helper()'
  'x = 42;'
  'end'
  });
__bytecode_artifact__('compile', script, [directory, '/artifact_script.nbc']);
info = __bytecode_artifact__('inspect', [directory, '/artifact_script.nbc']);
asserts.istrue(contains(info, 'functions=2'));
executableScript = [directory, '/compiled_script.m'];
write_lines(executableScript, {
  'artifact_value = helper();'
  'artifact_callback = @helper;'
  'function value = helper()'
  'value = 42;'
  'end'
  });
compiledScript = [directory, '/compiled_script.nbc'];
__bytecode_artifact__('compile', executableScript, compiledScript);
delete(executableScript);
originalDirectory = pwd();
run(compiledScript);
asserts.isequal(pwd(), originalDirectory);
asserts.isequal(artifact_value, 42);
asserts.isequal(artifact_callback(), 42);
clear artifact_value artifact_callback;
rmdir(directory, 's');

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  for k = 1:numel(lines)
    fprintf(fid, '%s\n', lines{k});
  end
  fclose(fid);
end

function bytes = read_bytes(filename)
  fid = fopen(filename, 'rb');
  bytes = fread(fid, Inf, '*uint8');
  fclose(fid);
end

function write_bytes(filename, bytes)
  fid = fopen(filename, 'wb');
  fwrite(fid, bytes, 'uint8');
  fclose(fid);
end

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
