%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_commands(varargin)
  [args, jsonOutput] = nmm_json_option(varargin);
  if length(args) > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  if isempty(args) || strcmp(args{1}, 'commands')
    if length(args) > 1
      nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
    end
    report = commandList();
    report = nmm_json_output(report, jsonOutput);
    return
  end
  if strcmp(args{1}, 'help')
    if length(args) == 1
      report = helpSummary();
    else
      report = commandHelp(args{2});
    end
    report = nmm_json_output(report, jsonOutput);
    return
  end
  nmm_error('UnknownCommandUnknownCommand', 'Unknown command.');
end
%=============================================================================
function report = helpSummary()
  commands = commandList();
  names = {};
  for k = 1:length(commands)
    names{end + 1} = commands(k).command;
  end
  report = struct();
  report.commands = names;
  report.usage = 'nmm(''help'', command)';
end
%=============================================================================
function report = commandHelp(command)
  if ~ischar(command)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  commands = commandList();
  for k = 1:length(commands)
    if strcmp(commands(k).command, command) || any(strcmp(commands(k).aliases, command))
      report = commands(k);
      return
    end
  end
  nmm_error('UnknownHelpCommand', 'Unknown nmm command: %s', command);
end
%=============================================================================
function commands = commandList()
  commands = struct([]);
  commands = addCommand(commands, 'list', 'installed modules', {"nmm('list')"}, {});
  commands = addCommand(commands, 'load', 'load an installed module', {"nmm('load', name)", "nmm('load', name, version)"}, {});
  commands = addCommand(commands, 'init', 'generate a valid module.json manifest', {"nmm('init', destination, 'title', t, 'summary', s, 'license', l)", "nmm('init', destination, 'Interactive', true)"}, {});
  commands = addCommand(commands, 'install', 'install source, archive or registry package', {"nmm('install', source)", "nmm('install', package, version)"}, {});
  commands = addCommand(commands, 'remove', 'remove an installed package', {"nmm('remove', name)", "nmm('remove', name, '-dry-run')"}, {'uninstall'});
  commands = addCommand(commands, 'pack', 'build a package archive', {"nmm('pack', path, destination)", "nmm('pack', path, '-dry-run')"}, {'package'});
  commands = addCommand(commands, 'publish', 'publish a package to a local registry', {"nmm('publish', file)", "nmm('publish', file, '-check')"}, {});
  commands = addCommand(commands, 'registry', 'inspect and validate registry state', {"nmm('registry', 'list')", "nmm('registry', 'doctor')"}, {'search', 'info', 'versions'});
  commands = addCommand(commands, 'cache', 'manage the offline archive cache', {"nmm('cache', 'list')", "nmm('cache', 'verify')"}, {});
  commands = addCommand(commands, 'lock', 'create or inspect module lockfiles', {"nmm('lock', path)", "nmm('lock', path, '-check')"}, {});
  commands = addCommand(commands, 'i18n', 'generate or refresh a module locale catalog', {"nmm('i18n', path)", "nmm('i18n', name, 'en_US', 'fr_FR')"}, {});
  commands = addCommand(commands, 'verify', 'verify archive or source module integrity', {"nmm('verify', file)", "nmm('verify', path)"}, {});
  commands = addCommand(commands, 'diagnostics', 'inspect dependencies and package state', {"nmm('status', name)", "nmm('tree', name)"}, {'why', 'deps', 'rdeps', 'doctor', 'audit', 'explain', 'graph'});
  commands = addCommand(commands, 'versions', 'inspect versions and semver constraints', {"nmm('installed', name)", "nmm('resolve', name, spec)"}, {'installed', 'latest', 'resolve', 'satisfies', 'pin', 'unpin'});
  commands = addCommand(commands, 'maintenance', 'repair, clean or update installed modules', {"nmm('repair')", "nmm('orphans')", "nmm('autoremove', '-dry-run')"}, {'outdated', 'update', 'config', 'autoload', 'orphans', 'autoremove'});
  commands = addCommand(commands, 'help', 'list commands or describe one command', {"nmm('commands')", "nmm('help', command)"}, {'commands'});
end
%=============================================================================
function commands = addCommand(commands, command, summary, syntax, aliases)
  item = struct();
  item.command = command;
  item.summary = summary;
  item.syntax = syntax;
  item.aliases = aliases;
  if isempty(commands)
    commands = item;
  else
    commands(end + 1) = item;
  end
end
%=============================================================================
