%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
workdir = tempname();
mkdir(workdir);
issueFile = [workdir, '/cli_issue_case.m'];
filewrite(issueFile, ['function y = cli_issue_case(x)', newline(), ...
  'y = x;   ', newline(), ...
  'end', newline()]);
okFile = [workdir, '/cli_ok_case.m'];
filewrite(okFile, ['function y = cli_ok_case(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);

lintCommand = nelson_tool_command_for_test('nelson-lint');

[status, output] = system([lintCommand, ' --format json "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"id":"NLS0010"'));
asserts.istrue(contains(output, '"ruleName":"trailing-whitespace"'));
asserts.istrue(contains(output, '"fingerprint"'));
asserts.istrue(contains(output, '"helpUri"'));
asserts.istrue(contains(output, '"category"'));
asserts.istrue(contains(output, '"fixes"'));
asserts.istrue(contains(output, '"replacementText":""'));

[status, output] = system([lintCommand, ' --format sarif "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"version":"2.1.0"'));
asserts.istrue(contains(output, '"ruleId":"NLS0010"'));
asserts.istrue(contains(output, '"rules"'));
asserts.istrue(contains(output, '"id":"NLS0033"'));
asserts.istrue(contains(output, '"name":"duplicate-classdef-event"'));
asserts.istrue(contains(output, '"defaultConfiguration":{"level":"warning"}'));
asserts.istrue(contains(output, '"id":"NLS0037"'));
asserts.istrue(contains(output, '"name":"duplicate-classdef-superclass"'));
asserts.istrue(contains(output, '"id":"NLS0038"'));
asserts.istrue(contains(output, '"name":"duplicate-classdef-attribute"'));
asserts.istrue(contains(output, '"category":"Classdef"'));
asserts.istrue(contains(output, '"fixes"'));

previousSarifFolder = pwd();
cleanupSarifFolder = onCleanup(@() cd(previousSarifFolder));
cd(workdir);
[status, output] = system([lintCommand, ' --format sarif "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"uri":"cli_issue_case.m"'));
cd(previousSarifFolder);
clear cleanupSarifFolder;

[status, output] = system([lintCommand, ' --format text "', okFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');

[status, output] = system([lintCommand, ' "', workdir, '/missing_case.m"']);
asserts.isequal(status, 2);
asserts.istrue(contains(output, 'path does not exist'));

configFile = [workdir, '/cli_config.json'];
filewrite(configFile, ['{', newline(), ...
  '  "version": 2,', newline(), ...
  '  "rules": { "NLS0010": { "level": "allow" } }', newline(), ...
  '}']);
[status, output] = system([lintCommand, ' --config "', configFile, '" "', issueFile, '"']);
asserts.isequal(status, 0);
asserts.isequal(output, '');

[status, output] = system([lintCommand, ' --fail-on error "', issueFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'NLS0010'));

[status, output] = system([lintCommand, ' --deny warnings "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, 'NLS0010'));

configCiError = [workdir, '/ci_error_config.json'];
filewrite(configCiError, ['{', newline(), ...
  '  "version": 2,', newline(), ...
  '  "ci": { "failOn": "error" }', newline(), ...
  '}']);
[status, output] = system([lintCommand, ' --config "', configCiError, '" "', issueFile, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'NLS0010'));

configCiWarning = [workdir, '/ci_warning_config.json'];
filewrite(configCiWarning, ['{', newline(), ...
  '  "version": 2,', newline(), ...
  '  "ci": { "failOn": "warning" }', newline(), ...
  '}']);
[status, output] = system([lintCommand, ' --config "', configCiWarning, '" "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, 'NLS0010'));

[status, output] = system([lintCommand, ' --quiet "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.isequal(output, '');

outputFile = [workdir, '/lint_output.txt'];
[status, output] = system([lintCommand, ' --output "', outputFile, '" "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.isequal(output, '');
asserts.istrue(contains(fileread(outputFile), 'NLS0010'));

[status, output] = system([lintCommand, ...
  ' --stdin --stdin-filename stdin_case.m --format json < "', issueFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"file":"stdin_case.m"'));
%=============================================================================
