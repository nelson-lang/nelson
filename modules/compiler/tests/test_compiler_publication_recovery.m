%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 180-->
ncc('--help');
check_build_recovery();

function check_build_recovery()
  work = tempname(fullfile(nelsonroot(), 'temp'));
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  entry = fullfile(work, 'recovered_app.m');
  output = fullfile(work, 'output');
  filewrite(entry, 'function recovered_app(); disp(''BEFORE_RECOVERY''); end');
  old = compiler.build.standaloneApplication(entry, 'OutputDir', output, 'EmbedArchive', false);
  report = jsondecode(fileread(fullfile(output, 'buildresult.json')));
  files = [old.Files; {fullfile(output, 'buildresult.json')}];
  inventory = cell(1, numel(files));
  transaction = fullfile(output, '.nelsonc-publish');
  mkdir(fullfile(transaction, 'new'));
  mkdir(fullfile(transaction, 'previous'));
  for k = 1:numel(files)
    [parent, stem, extension] = fileparts(files{k});
    inventory{k} = struct('relative', [stem, extension], 'sha256', sha256(files{k}, '-file'));
    copyfile(files{k}, fullfile(transaction, 'new'));
  end
  journal = struct('format', 1, 'kind', 'nelson-application-publication', ...
    'previous', {inventory}, 'next', {inventory});
  filewrite(fullfile(transaction, 'journal.json'), jsonencode(journal));
  movefile(fullfile(output, 'buildresult.json'), fullfile(transaction, 'previous'));
  movefile(old.Files{1}, fullfile(transaction, 'previous'));
  filewrite(fullfile(output, 'user.txt'), 'PRESERVE');
  filewrite(entry, 'function recovered_app(); disp(''AFTER_RECOVERY''); end');
  options = compiler.build.StandaloneApplicationOptions(entry, 'OutputDir', output);
  if ispc()
    result = compiler.build.standaloneWindowsApplication(options);
  else
    result = compiler.build.standaloneApplication(options);
  end
  asserts.isfalse(isfolder(transaction));
  asserts.isfalse(isfile(fullfile(output, 'recovered_app.nca')));
  asserts.isequal(fileread(fullfile(output, 'user.txt')), 'PRESERVE');
  asserts.isfalse(strcmp(sha256(result.Files{1}, '-file'), report.files(1).sha256));
  rmfile(entry);
  previousRuntime = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previousRuntime));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  [status, text] = system(['"', result.Files{1}, '"'], 60);
  asserts.istrue(status == 0, text);
  asserts.istrue(contains(text, 'AFTER_RECOVERY'), text);
  clear restore;
  clear cleanup;
end

function remove_work(work)
  allowed = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(work), [allowed, '/']));
  if isfolder(work); rmdir(work, 's'); end
end
