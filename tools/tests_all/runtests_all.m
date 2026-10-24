%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ver_number = version('-number');
platform_map.('woa64') = 'win-arm64';
platform_map.('win64') = 'win-x86-64';
platform_map.('win32') = 'win-x86-32';
platform_map.('glnxa64') = 'linux-x86-64';
platform_map.('glnxa32') = 'linux-x86-32';
platform_map.('maci64') = 'mac-x86-64';
platform_map.('maci32') = 'mac-x86-32';
platform_map.('maca64') = 'mac-arm64';
platform_map.('?') = 'unknown-platform';
platform = platform_map.(computer('arch'));
TESTS_RESULT_DIR = getenv('TESTS_RESULT_DIR');
if strcmp(TESTS_RESULT_DIR, '') == true
  TESTS_RESULT_DIR = nelsonroot();
end
stop_on_fail = false;
filebase = ['tests_all-', mat2str(ver_number(1)), '.', mat2str(ver_number(2)), '.', mat2str(ver_number(3)), '.', mat2str(ver_number(4)), '-', platform];
xmlFile = [TESTS_RESULT_DIR, '/', filebase, '.xml'];
jsonFile = [TESTS_RESULT_DIR, '/', filebase, '.json'];
try
  shard_index = getPositiveIntegerEnv('NELSON_TEST_SHARD_INDEX', 1);
  shard_count = getPositiveIntegerEnv('NELSON_TEST_SHARD_COUNT', 1);
  % one extra worker overlaps child process startup I/O with test execution
  % (measured ~19% faster on a 2-core runner); NELSON_TEST_WORKERS overrides.
  workers = getPositiveIntegerEnv('NELSON_TEST_WORKERS', maxNumCompThreads() + 1);
  if shard_index > shard_count
    error('NELSON_TEST_SHARD_INDEX must be lower than or equal to NELSON_TEST_SHARD_COUNT.');
  end
  if strcmp(getenv('NELSON_TESTS_ALL_VALIDATE_OPTIONS_ONLY'), 'TRUE')
    disp(['NELSON_TEST_SHARD_INDEX=', int2str(shard_index)]);
    disp(['NELSON_TEST_SHARD_COUNT=', int2str(shard_count)]);
    disp(['NELSON_TEST_WORKERS=', int2str(workers)]);
    r = true;
  else
    if shard_count > 1
      s = nelson.unittest.discover([], 'Kind', 'tests');
      p = nelson.unittest.plan(s, 'ShardIndex', shard_index, 'ShardCount', shard_count, 'Workers', workers);
      disp(['=== Shard ', int2str(shard_index), '/', int2str(shard_count), ...
        ' — ', int2str(length(p.cases)), ' tests (of ', int2str(s.count), ' total) ===']);
      results = nelson.unittest.run(p, ...
        'Format', 'junit', ...
        'OutputFile', xmlFile, ...
        'StopOnFail', stop_on_fail);
    else
      results = nelson.unittest.run([], ...
        'Kind', 'tests', ...
        'Workers', workers, ...
        'Format', 'junit', ...
        'OutputFile', xmlFile, ...
        'StopOnFail', stop_on_fail);
    end
    filewrite(jsonFile, jsonprettyprint(jsonencode(results.raw)));
    r = results.status;
  end
catch
  m = lasterror();
  disp(m);
  r = false;
end
if ispc()
  if r == false
    exit(1);
  else
    exit();
  end
else
  exit();
end
%=============================================================================

function value = getPositiveIntegerEnv(name, defaultValue)
  raw = getenv(name);
  if strcmp(raw, '')
    value = defaultValue;
    return
  end
  value = str2double(raw);
  if isnan(value) || value < 1 || fix(value) ~= value
    error([name, ' must be a positive integer.']);
  end
end
%=============================================================================
