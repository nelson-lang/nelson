%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
nameMismatch = fullfile(fixtureDirectory, 'nelson_lint_name_mismatch.m');
filewrite(nameMismatch, ['function y = wrong_lint_name(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
fixDiagnostics = __code_analyzer__(nameMismatch, false, '', false, true, false, false);
fixIds = {fixDiagnostics.CheckID};
nameMismatchIndex = find(strcmp(fixIds, 'NLS0002'), 1);
asserts.istrue(~isempty(nameMismatchIndex));
asserts.isequal(fixDiagnostics(nameMismatchIndex).Fixability, 'safe');
asserts.istrue(numel(fixDiagnostics(nameMismatchIndex).Fixes) >= 1);
asserts.isequal(fixDiagnostics(nameMismatchIndex).Fixes(1).ReplacementText, ...
  'nelson_lint_name_mismatch');

classNameMismatch = fullfile(fixtureDirectory, 'NelsonLintClassNameMismatch.m');
filewrite(classNameMismatch, ['classdef (Sealed) WrongClassName', newline(), ...
  'end', newline()]);
fixDiagnostics = __code_analyzer__(classNameMismatch, false, '', false, true, false, false);
fixIds = {fixDiagnostics.CheckID};
classNameMismatchIndex = find(strcmp(fixIds, 'NLS0002'), 1);
asserts.istrue(~isempty(classNameMismatchIndex));
asserts.istrue(contains(fixDiagnostics(classNameMismatchIndex).Message, 'Class name'));
asserts.isequal(fixDiagnostics(classNameMismatchIndex).Fixability, 'safe');
asserts.istrue(numel(fixDiagnostics(classNameMismatchIndex).Fixes) >= 1);
asserts.isequal(fixDiagnostics(classNameMismatchIndex).Fixes(1).ReplacementText, ...
  'NelsonLintClassNameMismatch');

classNameMatching = fullfile(fixtureDirectory, 'NelsonLintClassNameMatching.m');
filewrite(classNameMatching, ['classdef NelsonLintClassNameMatching', newline(), ...
  'end', newline()]);
issues = checkcode(classNameMatching, '-struct', '-id');
asserts.isfalse(any(strcmp({issues.id}, 'NLS0002')));

professional = fullfile(fixtureDirectory, 'nelson_lint_professional.m');
filewrite(professional, ['function y = nelson_lint_professional(x, onlyInText)', newline(), ...
  'a = 1;', newline(), ...
  'msg = ''a onlyInText''; % onlyInText is not a use', newline(), ...
  'y = x;', newline(), ...
  'end', newline(), ...
  'function z = local_case(k, unusedLocal)', newline(), ...
  'z = k;', newline(), ...
  'end', newline(), ...
  'function z = unused_local_case(k)', newline(), ...
  'z = k;', newline(), ...
  'end', newline()]);
issues = checkcode(professional, '-struct', '-id');
ids = {issues.id};
asserts.istrue(any(strcmp(ids, 'NLS0004')));
asserts.istrue(any(strcmp(ids, 'NLS0007')));
asserts.istrue(any(strcmp(ids, 'NLS0023')));
inputIssues = issues(strcmp(ids, 'NLS0007'));
asserts.istrue(any([inputIssues.line] == 1));
asserts.istrue(any([inputIssues.line] == 6));
asserts.istrue(any(contains({inputIssues.message}, '''onlyInText''')));
diagnostics = __code_analyzer__(professional, false, '', false, true, false, false);
unusedAssignmentMatches = strcmp({diagnostics.CheckID}, 'NLS0004');
unusedAssignmentMatches = unusedAssignmentMatches & contains({diagnostics.Message}, '''a''');
unusedAssignmentIndex = find(unusedAssignmentMatches, 1);
asserts.istrue(~isempty(unusedAssignmentIndex));
asserts.isequal(diagnostics(unusedAssignmentIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(unusedAssignmentIndex).Fixes) >= 1);
asserts.isequal(diagnostics(unusedAssignmentIndex).Fixes(1).LineStart, 2);
asserts.isequal(diagnostics(unusedAssignmentIndex).Fixes(1).LineEnd, 3);
unusedLocalMatches = strcmp({diagnostics.CheckID}, 'NLS0023');
unusedLocalMatches = unusedLocalMatches & contains({diagnostics.Message}, '''unused_local_case''');
unusedLocalIndex = find(unusedLocalMatches, 1);
asserts.istrue(~isempty(unusedLocalIndex));
asserts.isequal(diagnostics(unusedLocalIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(unusedLocalIndex).Fixes) >= 1);
asserts.isequal(diagnostics(unusedLocalIndex).Fixes(1).LineStart, 9);
asserts.isequal(diagnostics(unusedLocalIndex).Fixes(1).LineEnd, 12);

previousLineSuppression = fullfile(fixtureDirectory, 'nls_prev.m');
filewrite(previousLineSuppression, ['function y = nls_prev(x)', newline(), ...
  '%#ok<NLS0012>', newline(), ...
  'veryLongVariableName = 1234567890123456789012345678901234567890;', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
configShortLine = fullfile(fixtureDirectory, 'nelson_lint_short_line.json');
filewrite(configShortLine, ['{', newline(), ...
  '  "version": 2,', newline(), ...
  '  "rules": { "NLS0012": { "options": { "maxLineLength": 30 } } }', newline(), ...
  '}']);
issues = __code_analyzer__(previousLineSuppression, false, configShortLine, false, true, false, false);
asserts.isfalse(any(strcmp({issues.CheckID}, 'NLS0012')));

%=============================================================================
