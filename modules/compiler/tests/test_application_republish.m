%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 120-->
ncc('--help');
check_republication();

function check_republication()
  work = tempname(tempdir());
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  source = fullfile(work, 'staged');
  target = fullfile(work, 'output');
  mkdir(source);
  mkdir(target);
  names = {'app.bin', 'app.nca', 'readme.txt', 'buildresult.json'};
  expected = cell(1, numel(names));
  for k = 1:numel(names)
    filewrite(fullfile(target, names{k}), ['previous ', names{k}]);
    expected{k} = struct('relative', names{k}, 'sha256', sha256(fullfile(target, names{k}), '-file'));
    if ~strcmp(names{k}, 'app.nca')
      filewrite(fullfile(source, names{k}), ['next ', names{k}]);
    end
  end
  filewrite(fullfile(target, 'user.txt'), 'USER CONTENT');
  encoded = jsonencode(expected);
  publish = @() __nelsonc_executable__('publish', source, target, encoded);
  rejected(@() __nelsonc_executable__('publish', source, target, '[]'));
  invalid = expected;
  invalid{1}.relative = '../outside';
  rejected(@() __nelsonc_executable__('publish', source, target, jsonencode(invalid)));
  invalid = expected;
  invalid{1}.sha256 = repmat('0', 1, 64);
  rejected(@() __nelsonc_executable__('publish', source, target, jsonencode(invalid)));
  invalid = expected(1:3);
  rejected(@() __nelsonc_executable__('publish', source, target, jsonencode(invalid)));
  rejected(@() __nelsonc_executable__('publish', source, target, jsonencode([expected, expected(1)])));
  invalid = expected;
  invalid{1}.relative = 'folder/file';
  rejected(@() __nelsonc_executable__('publish', source, target, jsonencode(invalid)));
  lock = fullfile(target, '.nelsonc-publish');
  mkdir(lock);
  marker = fullfile(lock, 'recovery.txt');
  filewrite(marker, 'DO NOT REMOVE');
  rejected(publish);
  asserts.isequal(fileread(marker), 'DO NOT REMOVE');
  remove_work(lock);
  filewrite(fullfile(source, 'user.txt'), 'COLLISION');
  rejected(publish);
  rmfile(fullfile(source, 'user.txt'));
  for k = 1:numel(names)
    asserts.isequal(sha256(fullfile(target, names{k}), '-file'), expected{k}.sha256);
  end
  if ispc()
    assert_locked_rollback(work, source, target, encoded);
    for k = 1:numel(names)
      asserts.isequal(sha256(fullfile(target, names{k}), '-file'), expected{k}.sha256);
    end
  end
  publish();
  for k = [1, 3, 4]
    asserts.isequal(fileread(fullfile(target, names{k})), ['next ', names{k}]);
  end
  asserts.isfalse(isfile(fullfile(target, 'app.nca')));
  asserts.isequal(fileread(fullfile(target, 'user.txt')), 'USER CONTENT');
  asserts.isfalse(isfolder(lock));
  rejected(publish);
  clear cleanup;
end

function assert_locked_rollback(work, source, target, encoded)
  script = fullfile(work, 'locked_publish.m');
  call = sprintf('__nelsonc_executable__(''publish'', %s, %s, %s);', quote(source), quote(target), quote(encoded));
  lines = {'try', 'ncc(''--help'');', 'failed = false;', 'try', call, ...
    'catch ex', 'failed = true; disp(ex.message);', 'end', ...
    'asserts.istrue(failed);', 'disp(''LOCKED_REPUBLISH_OK'');', 'exit(0);', ...
    'catch ex', 'disp(ex.message); exit(1);', 'end'};
  filewrite(script, strjoin(lines, char(10)));
  helper = fullfile(nelsonroot(), 'modules', 'compiler', 'tests', 'test_application_republish_lock.ps1');
  launcher = fullfile(modulepath('nelson', 'bin'), 'NelSon-cli.exe');
  command = ['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', helper, '" -LockedFile "', fullfile(target, 'readme.txt'), ...
    '" -Script "', script, '" -Launcher "', launcher, '"'];
  [status, output] = system(command, 90);
  asserts.istrue(status == 0, output);
  asserts.istrue(contains(output, 'LOCKED_REPUBLISH_OK'), output);
  asserts.isfalse(isfolder(fullfile(target, '.nelsonc-publish')));
end

function value = quote(value)
  value = ['''', strrep(value, '''', ''''''), ''''];
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end

function remove_work(work)
  allowed = tempdir();
  asserts.istrue(startsWith(fullpath(work), allowed));
  if isfolder(work); rmdir(work, 's'); end
end
