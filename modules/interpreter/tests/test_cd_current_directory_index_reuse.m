%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% cd keeps the function index of the directories it leaves and reuses it on
% return while the directory did not change. Entries added or removed in the
% meantime must still be seen when the directory becomes current again.
%=============================================================================
folderA = tempname();
mkdir(folderA);
folderB = tempname();
mkdir(folderB);
previousDir = pwd();
%=============================================================================
filewrite(fullfile(folderA, 'cd_reuse_probe_one.m'), ...
  {'function r = cd_reuse_probe_one()'; '  r = 1;'; 'end'});
cd(folderA);
folderA = pwd();
asserts.isequal(cd_reuse_probe_one(), 1);
%=============================================================================
% leaving and coming back resolves the same functions
cd(folderB);
asserts.isequal(which('cd_reuse_probe_one'), '');
cd(folderA);
asserts.isequal(cd_reuse_probe_one(), 1);
asserts.istrue(endsWith(which('cd_reuse_probe_one'), 'cd_reuse_probe_one.m'));
%=============================================================================
% a function added while away is found on return
cd(folderB);
filewrite(fullfile(folderA, 'cd_reuse_probe_two.m'), ...
  {'function r = cd_reuse_probe_two()'; '  r = 2;'; 'end'});
cd(folderA);
asserts.isequal(cd_reuse_probe_two(), 2);
%=============================================================================
% a function removed while away is gone on return
cd(folderB);
delete(fullfile(folderA, 'cd_reuse_probe_one.m'));
cd(folderA);
asserts.isequal(which('cd_reuse_probe_one'), '');
%=============================================================================
% run() moves into the script directory and back: the current directory
% resolves its functions afterwards
filewrite(fullfile(folderB, 'cd_reuse_script.m'), {'cd_reuse_script_value = 3;'});
run(fullfile(folderB, 'cd_reuse_script.m'));
asserts.isequal(cd_reuse_script_value, 3);
asserts.isequal(pwd(), folderA);
asserts.isequal(cd_reuse_probe_two(), 2);
%=============================================================================
% a directory left behind may be removed meanwhile: later cd calls still work
folderC = tempname();
mkdir(folderC);
cd(folderC);
cd(folderB);
rmdir(folderC);
cd(folderA);
asserts.isequal(cd_reuse_probe_two(), 2);
cd(folderB);
asserts.isequal(which('cd_reuse_probe_two'), '');
%=============================================================================
% the cache is bounded: many directories visited in between do not break it
folders = cell(1, 10);
for k = 1:10
  folders{k} = tempname();
  mkdir(folders{k});
  cd(folders{k});
end
cd(folderA);
asserts.isequal(cd_reuse_probe_two(), 2);
%=============================================================================
cd(previousDir);
