%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function args = __nelsonc_option_arguments__(options)
  if ~isa(options, 'nelson.compiler.BuildOptions') || ~isscalar(options)
    error('Nelson:nelsonc:options:scalarNelsonCompilerBuildOptionsObject', 'A scalar nelson.compiler.BuildOptions object is required.');
  end
  extension = '';
  if ispc()
    extension = '.exe';
  end
  output = fullfile(options.OutputDir, [options.ExecutableName, extension]);
  args = {options.AppFile, '-o', output, '--runtime', options.RuntimeMode};
  args(end + 1:end + 2) = {'--executable-version', options.ExecutableVersion};
  for k = 1:numel(options.SupportPackages)
    args(end + 1:end + 2) = {'--support-package', options.SupportPackages{k}};
  end
  if ~isempty(options.ExecutableIcon)
    args(end + 1:end + 2) = {'--executable-icon', options.ExecutableIcon};
  end
  if ~isempty(options.RuntimeLogFile)
    args(end + 1:end + 2) = {'--runtime-log-file', options.RuntimeLogFile};
  end
  if ~strcmp(options.Mode, 'auto')
    args{end + 1} = ['--', options.Mode];
  end
  if options.NoConsole
    args{end + 1} = '--no-console';
  end
  if ~options.EmbedArchive
    args{end + 1} = '--external-archive';
  end
  if ~options.AutoDetectDataFiles
    args{end + 1} = '-X';
  end
  if options.TreatInputsAsNumeric
    args{end + 1} = '-n';
  end
  if ~isempty(options.CustomHelpTextFile)
    args(end + 1:end + 2) = {'--help-file', options.CustomHelpTextFile};
  end
  for k = 1:numel(options.AdditionalFiles)
    args(end + 1:end + 2) = {'-a', options.AdditionalFiles{k}};
  end
end
