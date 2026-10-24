%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc()
  skip_testsuite('This test requires Windows long-path support.');
end
enabled = 0;
try
  enabled = winqueryreg('HKEY_LOCAL_MACHINE', 'SYSTEM\CurrentControlSet\Control\FileSystem', 'LongPathsEnabled');
catch
end
if enabled ~= 1
  skip_testsuite('The host long-path policy is not enabled.');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'long_path_entry.m');
filewrite(entry, {'function long_path_entry()', 'disp(''LONG_PATH_APP_OK'');', 'end'});
outputDirectory = directory;
while length(outputDirectory) < 225
  outputDirectory = fullfile(outputDirectory, 'nested_output');
end
output = fullfile(outputDirectory, 'long_path_app.exe');
ncc(entry, '-o', output);
runtimeDirectory = fullfile(outputDirectory, 'long_path_app.runtime');
asserts.istrue(length(fullfile(runtimeDirectory, 'bin', 'x64', 'libnlsElementary_functions_builtin.dll')) > 260);
inventory = jsondecode(fileread(fullfile(runtimeDirectory, 'runtime.json')));
checkedLongFile = false;
for k = 1:numel(inventory.files)
  filename = fullfile(runtimeDirectory, inventory.files(k).relative);
  if length(filename) > 260
    asserts.istrue(isfile(filename));
    asserts.isequal(lower(sha256(filename, '-file')), lower(inventory.files(k).sha256));
    checkedLongFile = true;
    break;
  end
end
asserts.istrue(checkedLongFile);
delete(entry);
[status, outputText] = system(['"', output, '" 2>&1'], 60);
asserts.istrue(status == 0, outputText);
asserts.istrue(contains(outputText, 'LONG_PATH_APP_OK'));
clear cleanup;
