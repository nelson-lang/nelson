%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_inno_build__(engine, stage, payload, shortcut, options, generator, kind)
  if nargin < 7
    kind = 'application';
  end
  if ~any(strcmp(kind, {'application', 'runtime', 'raw'}))
    error('Nelson:nelsonc:installer:unknownNativeInstallerKind', 'Unknown native installer kind.');
  end
  wrappedInstaller = ~strcmp(kind, 'raw');
  if wrappedInstaller
    launcher = fullfile(modulepath('nelson', 'bin'), 'nelson-installer.exe');
    if ~isfile(launcher)
      error('Nelson:nelsonc:installerTool', 'The precompiled installer launcher is missing.');
    end
  end
  compiled = fullfile(stage, 'compiled');
  mkdir(compiled);
  script = fullfile(stage, 'installer.iss');
  if nargin < 6
    lines = __nelsonc_inno_script__(payload, compiled, shortcut, options);
  else
    lines = generator(compiled);
  end
  filewrite(script, [char(65279), strjoin(lines, char(10))]);
  [status, output] = system(['"', engine, '" /Q "', script, '"'], 1800);
  if options.Verbose || status ~= 0
    fprintf('%s\n', output);
  end
  if status ~= 0
    if output_matches_antivirus(output)
      error('Nelson:nelsonc:installerTool:antivirusBlocked', 'Installer compilation was blocked by antivirus software (likely a false positive on the unsigned launcher) with status %d.\n%s', status, strtrim(output));
    end
    error('Nelson:nelsonc:installerTool:installerCompilationFailed', 'Installer compilation failed with status %d.\n%s', status, strtrim(output));
  end
  name = [options.InstallerName, '.exe'];
  executable = fullfile(compiled, name);
  if ~isfile(executable)
    error('Nelson:nelsonc:installerTool:installerCompilationProducedNoExecutable', 'Installer compilation produced no executable.');
  end
  if wrappedInstaller
    wrapped = fullfile(stage, 'wrapped');
    mkdir(wrapped);
    target = fullfile(wrapped, name);
    operation = 'wrap-installer';
    if strcmp(kind, 'runtime'); operation = 'wrap-runtime-installer'; end
    __nelsonc_executable__(operation, launcher, executable, target);
    compiled = wrapped;
    executable = target;
  end
  info = dir(executable);
  publish = fullfile(stage, 'publish');
  mkdir(publish);
  if strcmp(options.PackageType, 'zip') || info.bytes >= 2 ^ 31
    name = [options.InstallerName, '.zip'];
    __nelsonc_archive__('pack', fullfile(publish, name), {[options.InstallerName, '.exe']}, [compiled, '/']);
  else
    [ok, message] = movefile(executable, fullfile(publish, name));
    if ~ok
      error('Nelson:nelsonc:installer', '%s', message);
    end
  end
  if ~isfolder(options.OutputDir)
    mkdir(options.OutputDir);
  end
  __nelsonc_executable__('publish', publish, options.OutputDir);
  if options.Verbose
    fprintf('Created %s\n', fullfile(options.OutputDir, name));
  end
end
%=============================================================================
function tf = output_matches_antivirus(output)
  % Inno Setup aborts with status 2 when the host antivirus blocks the freshly
  % compiled installer (it embeds the unsigned launcher). The wording varies by
  % product and locale, so match ASCII-safe fragments common to those messages.
  signatures = {'potentially unwanted', 'potentiellement', 'virus'};
  haystack = lower(output);
  tf = false;
  for k = 1:numel(signatures)
    if contains(haystack, signatures{k})
      tf = true;
      return;
    end
  end
end
%=============================================================================
