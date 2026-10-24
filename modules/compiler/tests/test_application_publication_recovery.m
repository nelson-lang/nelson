%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
% <--TIMEOUT 180-->
ncc('--help');
check_recovery();

function check_recovery()
  work = tempname(tempdir());
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  for phase = 0:6
    state = fixture(work, sprintf('phase%d', phase));
    move_old(state, min(phase, 4));
    if phase == 3; publish_one(state, 1); end
    if phase >= 4
      for k = 1:numel(state.Next); publish_one(state, k); end
    end
    if phase == 5; rmfile(fullfile(state.Transaction, 'previous', state.Previous{1}.relative)); end
    if phase == 6
      % A recovery was itself interrupted after restoring the first old output.
      rmfile(fullfile(state.Target, state.Next{2}.relative));
      rmfile(fullfile(state.Target, state.Next{1}.relative));
      movefile(fullfile(state.Transaction, 'previous', state.Previous{1}.relative), state.Target);
    end
    __nelsonc_executable__('recover-publication', state.Target);
    if phase == 4 || phase == 5
      verify_files(state.Target, state.Next);
      asserts.isfalse(isfile(fullfile(state.Target, 'app.nca')));
    else
      verify_files(state.Target, state.Previous);
    end
    asserts.isequal(fileread(fullfile(state.Target, 'user.txt')), 'UNRELATED');
    asserts.isfalse(isfolder(state.Transaction));
    asserts.istrue(isfile(fullfile(state.Target, '.nelsonc-publish.lock')));
    __nelsonc_executable__('recover-publication', state.Target);
  end
  check_refusals(work);
  state = fixture(work, 'initial');
  for k = 1:numel(state.Previous); rmfile(fullfile(state.Target, state.Previous{k}.relative)); end
  journal = jsondecode(fileread(fullfile(state.Transaction, 'journal.json')));
  journal.previous = {};
  filewrite(fullfile(state.Transaction, 'journal.json'), jsonencode(journal));
  publish_one(state, 1);
  __nelsonc_executable__('recover-publication', state.Target);
  asserts.isfalse(isfile(fullfile(state.Target, 'app.bin')));
  asserts.isequal(fileread(fullfile(state.Target, 'user.txt')), 'UNRELATED');
  mkdir(state.Transaction);
  __nelsonc_executable__('recover-publication', state.Target);
  asserts.isfalse(isfolder(state.Transaction));
  if ispc(); native_recovery(work); end
  clear cleanup;
end

function check_refusals(work)
  for damage = 1:7
    state = fixture(work, sprintf('damaged%d', damage));
    move_old(state, 4);
    publish_one(state, 1);
    journalFile = fullfile(state.Transaction, 'journal.json');
    journal = jsondecode(fileread(journalFile));
    switch damage
      case 1; filewrite(fullfile(state.Target, 'app.bin'), 'USER EDIT');
      case 2; filewrite(fullfile(state.Transaction, 'previous', 'readme.txt'), 'BAD BACKUP');
      case 3; filewrite(fullfile(state.Transaction, 'new', 'extra.txt'), 'UNMANAGED');
      case 4; journal.previous(1).relative = '../outside.txt'; filewrite(journalFile, jsonencode(journal));
      case 5; journal.format = 2; filewrite(journalFile, jsonencode(journal));
      case 6; filewrite(journalFile, '{incomplete');
      case 7; rmfile(fullfile(state.Transaction, 'previous', 'readme.txt'));
    end
    before = tree_snapshot(state.Target);
    asserts.istrue(numel(before) > 3);
    rejected(@() __nelsonc_executable__('recover-publication', state.Target));
    asserts.isequal(tree_snapshot(state.Target), before);
    asserts.istrue(isfolder(state.Transaction));
  end
end

function state = fixture(work, name)
  target = fullfile(work, name);
  transaction = fullfile(target, '.nelsonc-publish');
  mkdir(fullfile(transaction, 'previous'));
  mkdir(fullfile(transaction, 'new'));
  previousNames = {'app.bin', 'app.nca', 'readme.txt', 'buildresult.json'};
  nextNames = {'app.bin', 'readme.txt', 'buildresult.json'};
  previous = write_inventory(target, previousNames, 'OLD ');
  next = write_inventory(fullfile(transaction, 'new'), nextNames, 'NEW ');
  journal = struct('format', 1, 'kind', 'nelson-application-publication', ...
    'previous', {previous}, 'next', {next});
  filewrite(fullfile(transaction, 'journal.json'), jsonencode(journal));
  filewrite(fullfile(target, 'user.txt'), 'UNRELATED');
  state = struct('Target', target, 'Transaction', transaction, 'Previous', {previous}, 'Next', {next});
end

function entries = write_inventory(folder, names, prefix)
  entries = cell(1, numel(names));
  for k = 1:numel(names)
    filename = fullfile(folder, names{k});
    filewrite(filename, [prefix, names{k}]);
    entries{k} = struct('relative', names{k}, 'sha256', sha256(filename, '-file'));
  end
end

function move_old(state, count)
  for k = 1:count
    movefile(fullfile(state.Target, state.Previous{k}.relative), fullfile(state.Transaction, 'previous'));
  end
end

function publish_one(state, index)
  movefile(fullfile(state.Transaction, 'new', state.Next{index}.relative), state.Target);
end

function verify_files(folder, entries)
  for k = 1:numel(entries)
    asserts.isequal(sha256(fullfile(folder, entries{k}.relative), '-file'), entries{k}.sha256);
  end
end

function entries = tree_snapshot(folder)
  entries = {};
  for relative = {'', '.nelsonc-publish', '.nelsonc-publish/new', '.nelsonc-publish/previous'}
    files = dir(fullfile(folder, relative{1}));
    for k = 1:numel(files)
      if files(k).isdir || strcmp(files(k).name, '.nelsonc-publish.lock'); continue; end
      filename = fullfile(files(k).folder, files(k).name);
      entries{end + 1} = [filename, ':', sha256(filename, '-file')];
    end
  end
  entries = sort(entries);
end

function native_recovery(work)
  helper = fullfile(nelsonroot(), 'modules', 'compiler', 'tests', 'test_publication_process_recovery.ps1');
  launcher = fullfile(modulepath('nelson', 'bin'), 'NelSon-cli.exe');
  [status, output] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', helper, '" -Work "', work, '" -Launcher "', launcher, '"'], 140);
  asserts.istrue(status == 0, output);
  asserts.istrue(contains(output, 'PROCESS_RECOVERY_OK'), output);
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
