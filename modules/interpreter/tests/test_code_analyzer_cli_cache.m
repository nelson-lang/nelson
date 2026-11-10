%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
workdir = tempname();
mkdir(workdir);
lintCommand = nelson_tool_command_for_test('nelson-lint');
%=============================================================================
cacheDir = [workdir, '/cache'];
mkdir(cacheDir);
previousCacheDir = getenv('NELSON_CODE_ANALYZER_CACHE_DIR');
cleanupCacheDir = onCleanup(@() setenv('NELSON_CODE_ANALYZER_CACHE_DIR', previousCacheDir));
setenv('NELSON_CODE_ANALYZER_CACHE_DIR', cacheDir);
cacheRoot = [workdir, '/cache_root'];
mkdir(cacheRoot);
mkdir([cacheRoot, '/sub']);
cacheFile = [cacheRoot, '/sub/cli_cache_case.m'];
filewrite(cacheFile, ['function y = cli_cache_case(x)', newline(), ...
  'y = x;   ', newline(), ...
  'end', newline()]);
[status, output] = system([lintCommand, ' --include-subfolders true --format json "', cacheRoot, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"id":"NLS0010"'));
cacheEntries = dir([cacheDir, '/*.json']);
asserts.istrue(length(cacheEntries) >= 1);
cacheText = fileread([cacheEntries(1).folder, '/', cacheEntries(1).name]);
asserts.istrue(contains(cacheText, '"version":7'));
asserts.istrue(contains(cacheText, '"contentHash"'));
asserts.istrue(contains(cacheText, '"build":"'));
% An entry from the same build is reused as stored.
cacheEntryFile = [cacheEntries(1).folder, '/', cacheEntries(1).name];
emptiedText = regexprep(cacheText, '"diagnostics":\[.*\],"fileSize"', '"diagnostics":[],"fileSize"');
asserts.isfalse(strcmp(emptiedText, cacheText));
filewrite(cacheEntryFile, emptiedText);
[status, output] = system([lintCommand, ' --include-subfolders true --format json "', cacheRoot, '"']);
asserts.isequal(status, 0);
asserts.isfalse(contains(output, '"id":"NLS0010"'));
% An entry from another analyzer build is ignored and rewritten.
otherBuildText = regexprep(emptiedText, '"build":"[^"]*"', '"build":"other analyzer build"');
asserts.isfalse(strcmp(otherBuildText, emptiedText));
filewrite(cacheEntryFile, otherBuildText);
[status, output] = system([lintCommand, ' --include-subfolders true --format json "', cacheRoot, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"id":"NLS0010"'));
asserts.isequal(fileread(cacheEntryFile), cacheText);
previousFolder = pwd();
cleanupFolder = onCleanup(@() cd(previousFolder));
cd(cacheRoot);
[status, output] = system([lintCommand, ' --format json "sub/cli_cache_case.m"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"file":"sub/cli_cache_case.m"'));
cd(previousFolder);
clear cleanupFolder;
cacheConfigFile = [workdir, '/cache_config.json'];
filewrite(cacheConfigFile, ['{', newline(), ...
  '  "version": 2,', newline(), ...
  '  "rules": { "NLS0010": { "level": "allow" } }', newline(), ...
  '}']);
[status, output] = system([lintCommand, ' --config "', cacheConfigFile, ...
  '" --include-subfolders true --format text "', cacheRoot, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
configuredCacheEntries = dir([cacheDir, '/*.json']);
asserts.istrue(length(configuredCacheEntries) > length(cacheEntries));
filewrite(cacheFile, ['function y = cli_cache_case(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
[status, output] = system([lintCommand, ' --include-subfolders true --format text "', cacheRoot, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');
%=============================================================================
