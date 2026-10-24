%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
workdir = tempname();
mkdir(workdir);
lintCommand = nelson_tool_command_for_test('nelson-lint');
%=============================================================================
dryRunFile = [workdir, '/cli_dry_run_case.m'];
filewrite(dryRunFile, ['function y = cli_dry_run_case(x)', newline(), ...
  'y = x;   ', newline(), ...
  'end']);
[status, output] = system([lintCommand, ' --fix-dry-run "', dryRunFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '@@ safe code analyzer fixes @@'));
asserts.istrue(contains(output, 'NLS0010'));
asserts.istrue(contains(fileread(dryRunFile), 'y = x;   '));
%=============================================================================
diffFile = [workdir, '/cli_diff_case.m'];
filewrite(diffFile, ['function y = cli_diff_case(x)', newline(), ...
  'y = x;   ', newline(), ...
  'end']);
[status, output] = system([lintCommand, ' --diff "', diffFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '@@ safe code analyzer fixes @@'));
asserts.istrue(contains(output, 'NLS0010'));
asserts.istrue(contains(fileread(diffFile), 'y = x;   '));
%=============================================================================
fixFile = [workdir, '/cli_fix_case.m'];
filewrite(fixFile, ['function y = cli_fix_case(x)', newline(), ...
  'y = x;   ', newline(), ...
  'end']);
[status, output] = system([lintCommand, ' --fix "', fixFile, '"']);
asserts.isequal(status, 0);
fixedText = fileread(fixFile);
asserts.isfalse(contains(fixedText, [';   ', newline()]));
asserts.istrue(endsWith(fixedText, newline()));
%=============================================================================
nameFixFile = [workdir, '/cli_expected_name.m'];
filewrite(nameFixFile, ['function y = cli_actual_name(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
[status, output] = system([lintCommand, ' --fix "', nameFixFile, '"']);
asserts.isequal(status, 0);
nameFixedText = fileread(nameFixFile);
asserts.istrue(contains(nameFixedText, 'function y = cli_expected_name(x)'));
asserts.isfalse(contains(nameFixedText, 'cli_actual_name'));
%=============================================================================
% Removing a whole line only stays safe for a complete simple statement. A
% block opener or a continued statement keeps the diagnostic but gets no
% automatic fix, because deleting that single line would unbalance the file.
blockOpenerFile = [workdir, '/cli_unreachable_block.m'];
filewrite(blockOpenerFile, ['function cli_unreachable_block(x)', newline(), ...
  'disp(x);', newline(), ...
  'return', newline(), ...
  'if x > 0', newline(), ...
  '  disp(1);', newline(), ...
  'end', newline(), ...
  'end', newline()]);
[status, output] = system([lintCommand, ' --format json "', blockOpenerFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"id":"NLS0003"'));
asserts.isfalse(contains(output, '"title":"Remove unreachable code"'));
[status, output] = system([lintCommand, ' --fix "', blockOpenerFile, '"']);
asserts.istrue(contains(fileread(blockOpenerFile), 'if x > 0'));
%=============================================================================
continuationFile = [workdir, '/cli_unreachable_continuation.m'];
filewrite(continuationFile, ['function y = cli_unreachable_continuation(x)', newline(), ...
  'y = x;', newline(), ...
  'return', newline(), ...
  'y = [1, ...', newline(), ...
  '  2];', newline(), ...
  'end', newline()]);
[status, output] = system([lintCommand, ' --format json "', continuationFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"id":"NLS0003"'));
asserts.isfalse(contains(output, '"title":"Remove unreachable code"'));
[status, output] = system([lintCommand, ' --fix "', continuationFile, '"']);
asserts.istrue(contains(fileread(continuationFile), 'y = [1, ...'));
%=============================================================================
plainUnreachableFile = [workdir, '/cli_unreachable_plain.m'];
filewrite(plainUnreachableFile, ['function y = cli_unreachable_plain(x)', newline(), ...
  'y = x;', newline(), ...
  'return', newline(), ...
  'y = x + 1;', newline(), ...
  'end', newline()]);
[status, output] = system([lintCommand, ' --format json "', plainUnreachableFile, '"']);
asserts.isequal(status, 1);
asserts.istrue(contains(output, '"title":"Remove unreachable code"'));
[status, output] = system([lintCommand, ' --fix "', plainUnreachableFile, '"']);
asserts.isfalse(contains(fileread(plainUnreachableFile), 'y = x + 1;'));
%=============================================================================
