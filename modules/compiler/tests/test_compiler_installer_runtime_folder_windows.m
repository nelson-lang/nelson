%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
% <--TIMEOUT 900-->
ncc('--help');
try
if ispc(); check_runtime_folder(); end

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_runtime_folder()
  work = [tempname(fullfile(nelsonroot(), 'temp')), ' runtime '' ', char([28450, 23383])];
  mkdir(work);
  source = fullfile(work, 'source'); mkdir(source);
  entryA = fullfile(source, 'shared_a.m');
  entryB = fullfile(source, 'shared_b.m');
  filewrite(entryA, ['function shared_a(output); ', ...
    'if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end; ', ...
    'filewrite(output, nelsonroot()); disp(''SHARED_A_OK''); end']);
  filewrite(entryB, ['function shared_b(output); ', ...
    'if sin(0) ~= 0; error(''Invalid result.''); end; ', ...
    'filewrite(output, nelsonroot()); disp(''SHARED_B_OK''); end']);
  a = compiler.build.standaloneApplication(entryA, 'OutputDir', fullfile(work, 'build-a'), 'EmbedArchive', false);
  b = compiler.build.standaloneWindowsApplication(entryB, 'OutputDir', fullfile(work, 'build-b'));
  first = __nelsonc_distribution_plan__(a);
  second = __nelsonc_distribution_plan__(b);
  asserts.isfalse(any(strcmp(first.runtime.modules, 'trigonometric_functions')));
  asserts.istrue(any(strcmp(second.runtime.modules, 'trigonometric_functions')));
  compiler.package.installer(a, 'ApplicationName', 'Runtime folder A', 'RuntimeDelivery', 'installer', ...
    'InstallerName', 'first', 'OutputDir', fullfile(work, 'package-a'), 'Shortcut', '');
  compiler.package.installer(b, 'ApplicationName', 'Runtime folder B', 'RuntimeDelivery', 'installer', ...
    'InstallerName', 'second', 'OutputDir', fullfile(work, 'package-b'), 'Shortcut', '');
  compiler.package.installer(a, 'ApplicationName', 'Runtime folder A', 'RuntimeDelivery', 'none', ...
    'InstallerName', 'absent', 'OutputDir', fullfile(work, 'package-none'), 'Shortcut', '');
  check_reserved_inputs(work, a);
  rmdir(source, 's'); rmdir(a.Options.OutputDir, 's'); rmdir(b.Options.OutputDir, 's');
  helper = fullfile(modulepath('compiler'), 'tests', 'test_installer_runtime_folder_windows.ps1');
  [status, output] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', helper, '" -Work "', work, ...
    '" -Fingerprint ', first.engineFingerprint, ' -Architecture ', first.architecture], 480);
  asserts.istrue(status == 0, output);
  asserts.istrue(contains(output, 'WINDOWS_RUNTIME_FOLDER_OK'), output);
  disp(output);
  allowed = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(work), [allowed, '/']));
  rmdir(work, 's');
end

function check_reserved_inputs(work, result)
  names = {'.nelson-install', 'nelson-runtime-package.exe'};
  for k = 1:numel(names)
    reserved = fullfile(work, names{k}); filewrite(reserved, 'KEEP');
    rejected = false;
    try
      compiler.package.installer(result, 'RuntimeDelivery', 'none', 'AdditionalFiles', reserved, ...
        'OutputDir', fullfile(work, 'reserved-output'));
    catch ex
      rejected = (strcmp(ex.identifier, 'Nelson:nelsonc:installer') || startsWith(ex.identifier, 'Nelson:nelsonc:installer:'));
    end
    asserts.istrue(rejected);
    asserts.isequal(fileread(reserved), 'KEEP');
    asserts.isfalse(isfolder(fullfile(work, 'reserved-output')));
  end
end
