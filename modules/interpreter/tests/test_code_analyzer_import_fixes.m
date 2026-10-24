%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
singleLineImportScope = fullfile(fixtureDirectory, 'nelson_lint_single_line_import_scope.m');
singleLineImportLine = 'import nelson.classdefpkg.Options, nelson.classdefpkg.OtherUnused';
filewrite(singleLineImportScope, ['function y = nelson_lint_single_line_import_scope()', newline(), ...
  singleLineImportLine, newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
diagnostics = __code_analyzer__(singleLineImportScope, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
singleLineImportMatches = strcmp(ids, 'NLS0030');
singleLineImportMatches = singleLineImportMatches ...
  & contains({diagnostics.Message}, '''nelson.classdefpkg.OtherUnused''');
singleLineImportIndex = find(singleLineImportMatches, 1);
asserts.istrue(~isempty(singleLineImportIndex));
asserts.isequal(diagnostics(singleLineImportIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(singleLineImportIndex).Fixes) >= 1);
asserts.isequal(diagnostics(singleLineImportIndex).Fixes(1).LineStart, 2);
asserts.isequal(diagnostics(singleLineImportIndex).Fixes(1).LineEnd, 2);
asserts.isequal(diagnostics(singleLineImportIndex).Fixes(1).ColumnStart, ...
  strfind(singleLineImportLine, ', nelson.classdefpkg.OtherUnused'));
asserts.isequal(diagnostics(singleLineImportIndex).Fixes(1).ColumnEnd, ...
  length(singleLineImportLine) + 1);
%=============================================================================
continuationImportScope = fullfile(fixtureDirectory, 'nelson_lint_continuation_import_scope.m');
filewrite(continuationImportScope, ['function y = nelson_lint_continuation_import_scope()', newline(), ...
  'import nelson.classdefpkg.Options, ...', newline(), ...
  '  nelson.classdefpkg.OtherUnused', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
issues = checkcode(continuationImportScope, '-struct', '-id');
ids = {issues.id};
asserts.istrue(any(strcmp(ids, 'NLS0030') ...
  & contains({issues.message}, '''nelson.classdefpkg.OtherUnused''')));
asserts.isfalse(any(strcmp(ids, 'NLS0030') & contains({issues.message}, '''Options''')));
diagnostics = __code_analyzer__(continuationImportScope, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
continuedImportMatches = strcmp(ids, 'NLS0030');
continuedImportMatches = continuedImportMatches ...
  & contains({diagnostics.Message}, '''nelson.classdefpkg.OtherUnused''');
continuedImportIndex = find(continuedImportMatches, 1);
asserts.istrue(~isempty(continuedImportIndex));
asserts.isequal(diagnostics(continuedImportIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(continuedImportIndex).Fixes) >= 1);
asserts.isequal(diagnostics(continuedImportIndex).Fixes(1).LineStart, 2);
asserts.isequal(diagnostics(continuedImportIndex).Fixes(1).LineEnd, 4);
%=============================================================================
continuationImportCommentScope = fullfile(fixtureDirectory, 'nelson_lint_continuation_import_comment_scope.m');
filewrite(continuationImportCommentScope, ...
  ['function y = nelson_lint_continuation_import_comment_scope()', newline(), ...
  'import nelson.classdefpkg.Options, ... % keep note', newline(), ...
  '  nelson.classdefpkg.OtherUnused', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
diagnostics = __code_analyzer__(continuationImportCommentScope, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
continuedImportCommentMatches = strcmp(ids, 'NLS0030');
continuedImportCommentMatches = continuedImportCommentMatches ...
  & contains({diagnostics.Message}, '''nelson.classdefpkg.OtherUnused''');
continuedImportCommentIndex = find(continuedImportCommentMatches, 1);
asserts.istrue(~isempty(continuedImportCommentIndex));
asserts.isequal(diagnostics(continuedImportCommentIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(continuedImportCommentIndex).Fixes) >= 1);
asserts.isequal(diagnostics(continuedImportCommentIndex).Fixes(1).ReplacementText, ...
  [' % keep note', newline()]);
%=============================================================================
continuationLiteralImportScope = fullfile(fixtureDirectory, 'nelson_lint_continuation_literal_import_scope.m');
filewrite(continuationLiteralImportScope, ...
  ['function y = nelson_lint_continuation_literal_import_scope()', newline(), ...
  'import(''nelson.classdefpkg.Options'', ...', newline(), ...
  '  ''nelson.classdefpkg.OtherUnused'')', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
issues = checkcode(continuationLiteralImportScope, '-struct', '-id');
ids = {issues.id};
asserts.istrue(any(strcmp(ids, 'NLS0030') ...
  & contains({issues.message}, '''nelson.classdefpkg.OtherUnused''')));
asserts.isfalse(any(strcmp(ids, 'NLS0030') & contains({issues.message}, '''Options''')));
diagnostics = __code_analyzer__(continuationLiteralImportScope, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
continuedLiteralImportMatches = strcmp(ids, 'NLS0030');
continuedLiteralImportMatches = continuedLiteralImportMatches ...
  & contains({diagnostics.Message}, 'nelson.classdefpkg.OtherUnused');
continuedLiteralImportIndex = find(continuedLiteralImportMatches, 1);
asserts.istrue(~isempty(continuedLiteralImportIndex));
asserts.isequal(diagnostics(continuedLiteralImportIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(continuedLiteralImportIndex).Fixes) >= 1);
asserts.isequal(diagnostics(continuedLiteralImportIndex).Fixes(1).LineStart, 2);
asserts.isequal(diagnostics(continuedLiteralImportIndex).Fixes(1).LineEnd, 4);
asserts.isequal(diagnostics(continuedLiteralImportIndex).Fixes(1).ReplacementText, [')', newline()]);
%=============================================================================
