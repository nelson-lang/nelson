%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = __nelsonc_command__(args, invokingDirectory, displayOutput, structuredResult)
  if nargin() < 4
    structuredResult = false;
  end
  usage = 'usage: nelsonc entry.m -o application [--gui|--cli] [--no-console] [-a file] [-n|--numeric-inputs] [-X|--no-auto-data] [-C|--external-archive] [--support-package autodetect|none|name] [--runtime bundled|installed] [--executable-version version] [--executable-icon image] [--runtime-log-file file] [--help-file file] [--explain-link]';
  usage = [usage, newline(), 'Nelson: output = ncc(''entry.m'', ''-o'', ''application'', ...)'];
  for j = 1:numel(args)
    if isstring(args{j}) && isscalar(args{j})
      args{j} = char(args{j});
    elseif ~ischar(args{j}) || (~isrow(args{j}) && ~isempty(args{j}))
      error('Nelson:nelsonc:arguments:argumentsCharacterVectorsScalarStrings', 'Arguments must be character vectors or scalar strings.');
    end
  end
  if isempty(args)
    result = usage;
    if displayOutput
      fprintf('%s\n', usage);
    end
    return;
  end
  [entry, output, included, mode, explain, runtimeMode, helpFile, options, isHelp] = parse_inputs(args, invokingDirectory);
  if isHelp
    result = usage;
    if displayOutput; fprintf('%s\n', usage); end
    return;
  end
  if isempty(entry)
    error('Nelson:nelsonc:arguments:entryFileRequired', 'An entry file is required.');
  end
  if ~isempty(helpFile)
    options.helpText = __nelsonc_help_text__(helpFile);
  end
  if explain
    plan = __nelsonc_link_plan__(entry, included, options.autoDetectDataFiles, options.supportPackages);
    gui = __nelsonc_select_mode__(plan, mode);
    plan = __nelsonc_application_native__(plan);
    if strcmp(runtimeMode, 'installed')
      plan.runtime = jsondecode(__nelsonc_plan__('runtime', jsonencode(plan), gui));
    else
      plan.runtime = __nelsonc_runtime_plan__(plan, gui);
    end
    plan.runtime.mode = runtimeMode;
    plan.runtime.gui = gui;
    plan.noConsole = options.noConsole;
    plan.embedArchive = options.embedArchive;
    plan.treatInputsAsNumeric = options.treatInputsAsNumeric;
    plan.executableVersion = options.executableVersion;
    plan.executableIcon = options.executableIcon;
    plan.runtimeLogFile = options.runtimeLogFile;
    if isfield(options, 'helpText')
      plan.helpText = options.helpText;
    end
    result = plan;
    if displayOutput
      fprintf('%s\n', jsonencode(plan));
    end
    return;
  end
  if isempty(output)
    error('Nelson:nelsonc:arguments:outputExecutableRequired', 'An output executable is required (-o).');
  end
  if structuredResult
    result = __nelsonc_build__(entry, output, included, mode, runtimeMode, options);
  else
    __nelsonc_build__(entry, output, included, mode, runtimeMode, options);
    result = output;
  end
  if displayOutput
    fprintf('Created %s\n', output);
  end
end

function name = absolute_name(name, invokingDirectory)
  if isempty(name)
    error('Nelson:nelsonc:arguments:emptyPathNotAllowed', 'An empty path is not allowed.');
  end
  absolute = startsWith(name, '/') || startsWith(name, '\');
  if ispc()
    absolute = absolute || (length(name) > 2 && name(2) == ':');
  end
  if ~absolute
    name = fullfile(invokingDirectory, name);
  end
end

function [entry, output, included, mode, explain, runtimeMode, helpFile, options, isHelp] = parse_inputs(args, invokingDirectory)
  isHelp = false;
  entry = '';
  output = '';
  included = {};
  supportPackages = {};
  mode = 'auto';
  explain = false;
  runtimeMode = 'bundled';
  helpFile = '';
  options = struct('autoDetectDataFiles', true, 'noConsole', false, ...
    'executableVersion', '1.0.0.0', 'runtimeLogFile', '', 'executableIcon', '', 'embedArchive', true, 'treatInputsAsNumeric', false);
  k = 1;
  while k <= numel(args)
    argument = args{k};
    if any(strcmp(argument, {'-o', '-a', '--help-file', '--executable-icon'}))
      if k == numel(args)
        error('Nelson:nelsonc:arguments:missingValue', 'Missing value after %s.', argument);
      end
      k = k + 1;
      value = absolute_name(args{k}, invokingDirectory);
      if strcmp(argument, '-o')
        output = value;
      elseif strcmp(argument, '--help-file')
        helpFile = value;
      elseif strcmp(argument, '--executable-icon')
        options.executableIcon = __nelsonc_option_value__('ExecutableIcon', value);
      else
        included{end + 1} = value;
      end
    elseif any(strcmp(argument, {'--executable-version', '--runtime-log-file'}))
      if k == numel(args)
        error('Nelson:nelsonc:arguments:missingValue', 'Missing value after %s.', argument);
      end
      k = k + 1;
      if strcmp(argument, '--runtime-log-file')
        options.runtimeLogFile = __nelsonc_option_value__('RuntimeLogFile', args{k});
      else
        options.executableVersion = __nelsonc_option_value__('ExecutableVersion', args{k});
      end
    elseif strcmp(argument, '--runtime')
      if k == numel(args)
        error('Nelson:nelsonc:arguments:missingValueAfterRuntime', 'Missing value after --runtime.');
      end
      k = k + 1;
      runtimeMode = args{k};
      if ~any(strcmp(runtimeMode, {'bundled', 'installed'}))
        error('Nelson:nelsonc:arguments', 'Runtime mode must be bundled or installed.');
      end
    elseif strcmp(argument, '--support-package')
      if k == numel(args); error('Nelson:nelsonc:arguments:missingValueAfterSupportPackage', 'Missing value after --support-package.'); end
      k = k + 1;
      supportPackages{end + 1} = args{k};
    elseif strcmp(argument, '--gui') || strcmp(argument, '--cli')
      selected = argument(3:end);
      if ~strcmp(mode, 'auto') && ~strcmp(mode, selected)
        error('Nelson:nelsonc:arguments:guiCliMutuallyExclusive', '--gui and --cli are mutually exclusive.');
      end
      mode = selected;
    elseif strcmp(argument, '--explain-link')
      explain = true;
    elseif strcmp(argument, '--no-console')
      if ~ispc()
        error('Nelson:nelsonc:platform', 'No-console applications require Windows.');
      end
      options.noConsole = true;
    elseif strcmp(argument, '-X') || strcmp(argument, '--no-auto-data')
      options.autoDetectDataFiles = false;
    elseif strcmp(argument, '-C') || strcmp(argument, '--external-archive')
      options.embedArchive = false;
    elseif strcmp(argument, '-n') || strcmp(argument, '--numeric-inputs')
      options.treatInputsAsNumeric = true;
    elseif strcmp(argument, '--help') || strcmp(argument, '-h')
      isHelp = true;
      return;
    elseif startsWith(argument, '-')
      error('Nelson:nelsonc:arguments:unknownOption', 'Unknown option: %s.', argument);
    elseif isempty(entry)
      entry = absolute_name(argument, invokingDirectory);
    else
      error('Nelson:nelsonc:arguments:multipleEntryFiles', 'Only one entry file is allowed.');
    end
    k = k + 1;
  end
  if isempty(supportPackages); supportPackages = {'autodetect'}; end
  options.supportPackages = __nelsonc_option_value__('SupportPackages', supportPackages);
end
