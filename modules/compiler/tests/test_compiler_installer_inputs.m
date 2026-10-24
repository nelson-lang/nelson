%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
folder = fullfile(directory, 'assets');
mkdir(fullfile(folder, 'empty'));
filewrite(fullfile(folder, 'data.txt'), 'CONTENT');
input = fullfile(directory, 'one.txt');
filewrite(input, 'FIRST');
plan = jsondecode(__nelsonc_plan__('installer-inputs', jsonencode({folder, input, input})));
asserts.isequal(numel(plan.files), 2);
asserts.istrue(any(strcmp({plan.files.relative}, 'assets/data.txt')));
asserts.istrue(any(strcmp(plan.directories, 'assets/empty')));
for k = 1:numel(plan.files)
  asserts.isequal(sha256(plan.files(k).source, '-file'), plan.files(k).sha256);
end
mkdir(fullfile(directory, 'conflict'));
conflict = fullfile(directory, 'conflict', 'one.txt');
filewrite(conflict, 'SECOND');
for inputs = {{input, conflict}, {fullfile(directory, 'absent')}, {char(0)}}
  failed = false;
  try
    __nelsonc_plan__('installer-inputs', jsonencode(inputs{1}));
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
filewrite(conflict, 'FIRST');
equal = jsondecode(__nelsonc_plan__('installer-inputs', jsonencode({input, conflict})));
asserts.isequal(numel(equal.files), 1);
reserved = fullfile(directory, 'runtime.json');
filewrite(reserved, '{}');
failed = false;
try
  __nelsonc_plan__('installer-inputs', jsonencode({reserved}));
catch
  failed = true;
end
asserts.istrue(failed);
clear cleanup;
