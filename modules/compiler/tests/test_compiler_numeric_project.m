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
work = tempname();
mkdir(work);
cleanup = onCleanup(@() rmdir(work, 's'));
project = __nelsonc_project__('new');
project.Build.AppFile = fullfile(work, 'entry.m');
filewrite(project.Build.AppFile, 'function entry(varargin); end');
project.Build.TreatInputsAsNumeric = true;
project = __nelsonc_project__('validate', project);
filename = fullfile(work, 'numeric.ncproj');
__nelsonc_project__('save', project, filename);
asserts.isequal(__nelsonc_project__('load', filename), project);
asserts.istrue(__nelsonc_project__('options', project).TreatInputsAsNumeric);
asserts.istrue(contains(__nelsonc_project__('command', project), 'options.TreatInputsAsNumeric = true;'));
for invalid = {[], 2, NaN, 'enabled', [true false]}
  current = project;
  current.Build.TreatInputsAsNumeric = invalid{1};
  rejected(@() __nelsonc_project__('validate', current));
end
current = project;
current.Build = rmfield(current.Build, 'TreatInputsAsNumeric');
rejected(@() __nelsonc_project__('validate', current));
current.Version = 4;
current.Build = rmfield(current.Build, 'SupportPackages');
current.LastBuild.ReportFile = strrep(fullfile(work, 'buildresult.json'), '\', '/');
current.LastBuild.ReportSHA256 = repmat('a', 1, 64);
current.LastBuild.SourcesSHA256 = repmat('b', 1, 64);
current.LastBuild.SettingsSHA256 = __nelsonc_project_result__('fingerprint', current);
migrated = __nelsonc_project__('validate', current);
asserts.isequal(migrated.Version, 6);
asserts.isfalse(migrated.Build.TreatInputsAsNumeric);
asserts.isequal(migrated.LastBuild.ReportSHA256, current.LastBuild.ReportSHA256);
asserts.isequal(migrated.LastBuild.SourcesSHA256, current.LastBuild.SourcesSHA256);
asserts.isequal(migrated.LastBuild.SettingsSHA256, __nelsonc_project_result__('fingerprint', migrated));
filewrite(filename, jsonencode(current));
asserts.isequal(__nelsonc_project__('load', filename), migrated);
current.Build.Verbose = true;
altered = __nelsonc_project__('validate', current);
asserts.isequal(altered.LastBuild.SettingsSHA256, current.LastBuild.SettingsSHA256);
asserts.isfalse(strcmp(altered.LastBuild.SettingsSHA256, __nelsonc_project_result__('fingerprint', altered)));
filewrite(filename, jsonencode(current));
altered = __nelsonc_project__('load', filename);
asserts.isfalse(strcmp(altered.LastBuild.SettingsSHA256, __nelsonc_project_result__('fingerprint', altered)));
clear cleanup;

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
