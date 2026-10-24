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
if ~ispc(); return; end
work = tempname(); mkdir(work);
cleanup = onCleanup(@() rmdir(work, 's'));
options = compiler.package.InstallerOptions('ApplicationName', 'Script test', ...
  'RuntimeDelivery', 'installer', 'OutputDir', fullfile(work, 'output'));
payload = fullfile(work, 'payload'); mkdir(payload);
images = struct('InstallerIcon', '', 'InstallerLogo', '', 'AddRemoveProgramsIcon', '');
runtime = struct('Included', true, 'PrivateDirectory', 'app.runtime', ...
  'Fingerprint', repmat('a', 1, 64), 'Architecture', computer('arch'), ...
  'Package', fullfile(work, 'nelson-runtime-package.exe'));
lines = __nelsonc_inno_script__(payload, options.OutputDir, '', options, images, runtime);
sources = lines(startsWith(lines, 'Source:'));
asserts.isequal(numel(sources), 3);
asserts.istrue(contains(sources{1}, 'nelson-runtime-package.exe'));
asserts.istrue(contains(sources{1}, 'dontcopy nocompression'));
asserts.istrue(contains(sources{3}, 'recursesubdirs'));
runtime.Included = false;
lines = __nelsonc_inno_script__(payload, options.OutputDir, '', options, images, runtime);
sources = lines(startsWith(lines, 'Source:'));
asserts.isequal(numel(sources), 2);
asserts.isfalse(any(contains(sources, 'nelson-runtime-package.exe')));
stage = fullfile(work, 'invalid-script'); mkdir(stage);
failed = false;
try
  __nelsonc_inno_build__(__nelsonc_inno_engine__(), stage, payload, '', options, ...
    @(output){'[Setup]', 'UnknownSetting=yes'}, 'raw');
catch ex
  failed = strcmp(ex.identifier, 'Nelson:nelsonc:installerTool:installerCompilationFailed') ...
    && contains(ex.message, 'UnknownSetting') && contains(ex.message, 'installer.iss');
end
asserts.istrue(failed);
clear cleanup;
