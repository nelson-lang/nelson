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
check_project_limits();
asserts.checkerror('__nelsonc_project__(''load'', 42)', 'Expected a project filename.', 'Nelson:compiler:project:filenameExpected');
asserts.checkerror('__nelsonc_project__(''load'', ''project.txt'')', 'Compiler projects use the .ncproj extension.', 'Nelson:compiler:project:extensionExpected');

function check_project_limits()
  directory = tempname();
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  filename = fullfile(directory, 'saved.ncproj');
  project = __nelsonc_project__('new');
  __nelsonc_project__('save', project, filename);
  before = sha256(filename, '-file');
  large = project;
  large.Build.AdditionalFiles = repmat({fullfile(directory, repmat('a', 1, 180))}, 1, 7000);
  failed = false;
  try
    __nelsonc_project__('save', large, filename, true);
  catch ex
    failed = strcmp(ex.identifier, 'Nelson:compiler:project:fileTooLarge') && contains(ex.message, '1 MiB');
  end
  asserts.istrue(failed);
  asserts.isequal(sha256(filename, '-file'), before);
  asserts.isequal(__nelsonc_project__('load', filename), project);
  asserts.isequal(numel(dir(fullfile(directory, '*.ncproj'))), 1);
  largeFile = fullfile(directory, 'oversized.ncproj');
  filewrite(largeFile, repmat(' ', 1, 1024 * 1024 + 1));
  failed = false;
  try
    __nelsonc_project__('load', largeFile);
  catch ex
    failed = strcmp(ex.identifier, 'Nelson:compiler:project:fileTooLarge') && contains(ex.message, '1 MiB');
  end
  asserts.istrue(failed);
  clear cleanup;
end
