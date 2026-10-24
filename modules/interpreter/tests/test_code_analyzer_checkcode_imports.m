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
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
scriptDataflow = fullfile(fixtureDirectory, 'nelson_lint_script_dataflow.m');
filewrite(scriptDataflow, ['unusedScriptValue = 1;', newline(), ...
  'usedScriptValue = 2;', newline(), ...
  'disp(usedScriptValue);', newline()]);
issues = checkcode(scriptDataflow, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.istrue(any(strcmp(ids, 'NLS0004') & lineNumbers == 1));
asserts.isfalse(any(strcmp(ids, 'NLS0004') & lineNumbers == 2));
asserts.isfalse(any(strcmp(ids, 'NLS0027')));

scriptDynamic = fullfile(fixtureDirectory, 'nelson_lint_script_dynamic.m');
filewrite(scriptDynamic, ['eval(''x = 1'');', newline(), ...
  'load workspace_data.mat', newline(), ...
  'data = load(''workspace_data.mat'');', newline(), ...
  'msg = ''load(''''workspace_data.mat'''')'';', newline(), ...
  'disp(data);', newline()]);
issues = checkcode(scriptDynamic, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.istrue(any(strcmp(ids, 'NLS0022') & lineNumbers == 1));
asserts.istrue(any(strcmp(ids, 'NLS0022') & lineNumbers == 2));
asserts.isfalse(any(strcmp(ids, 'NLS0022') & lineNumbers == 3));
asserts.isfalse(any(strcmp(ids, 'NLS0022') & lineNumbers == 4));
diagnostics = __code_analyzer__(scriptDynamic, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
lineNumbers = [diagnostics.LineStart];
loadIndex = find(strcmp(ids, 'NLS0022') & lineNumbers == 2, 1);
asserts.isequal(diagnostics(loadIndex).Fixability, 'unsafe');
asserts.istrue(numel(diagnostics(loadIndex).Fixes) >= 1);
asserts.isfalse(diagnostics(loadIndex).Fixes(1).Safe);
asserts.isequal(diagnostics(loadIndex).Fixes(1).ReplacementText, ...
  ['loadedData = load(''workspace_data.mat'');', newline()]);

scriptLocalFunctions = fullfile(fixtureDirectory, 'nelson_lint_script_local_functions.m');
filewrite(scriptLocalFunctions, ['usedScriptValue = 3;', newline(), ...
  'disp(local_script_helper(usedScriptValue));', newline(), ...
  'function y = local_script_helper(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline(), ...
  'function y = unused_script_helper(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
issues = checkcode(scriptLocalFunctions, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.isfalse(any(strcmp(ids, 'NLS0004') & lineNumbers == 1));
asserts.isfalse(any(strcmp(ids, 'NLS0023') & lineNumbers == 3));
asserts.istrue(any(strcmp(ids, 'NLS0023') & lineNumbers == 6));

scriptImportScope = fullfile(fixtureDirectory, 'nelson_lint_script_import_scope.m');
filewrite(scriptImportScope, ['import nelson.classdefpkg.Options', newline(), ...
  'Options = 1;', newline(), ...
  'disp(Options);', newline()]);
issues = checkcode(scriptImportScope, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.istrue(any(strcmp(ids, 'NLS0029') & lineNumbers == 2));
asserts.isfalse(any(strcmp(ids, 'NLS0030')));

scriptUnusedImportScope = fullfile(fixtureDirectory, 'nelson_lint_script_unused_import_scope.m');
filewrite(scriptUnusedImportScope, ['import nelson.classdefpkg.Options', newline(), ...
  'disp(1);', newline()]);
issues = checkcode(scriptUnusedImportScope, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.istrue(any(strcmp(ids, 'NLS0030') & lineNumbers == 1));

importScope = fullfile(fixtureDirectory, 'nelson_lint_import_scope.m');
filewrite(importScope, ['function y = nelson_lint_import_scope()', newline(), ...
  'import nelson.classdefpkg.Options', newline(), ...
  'y = Options.empty();', newline(), ...
  'Options = y;', newline(), ...
  'end', newline()]);
issues = checkcode(importScope, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.isfalse(any(strcmp(ids, 'NLS0005') & contains({issues.message}, '''Options''')));
asserts.isfalse(any(strcmp(ids, 'NLS0004') & contains({issues.message}, '''Options''')));
asserts.istrue(any(strcmp(ids, 'NLS0029') & lineNumbers == 4));
asserts.isfalse(any(strcmp(ids, 'NLS0030')));

unusedImportScope = fullfile(fixtureDirectory, 'nelson_lint_unused_import_scope.m');
filewrite(unusedImportScope, ['function y = nelson_lint_unused_import_scope(x)', newline(), ...
  'import nelson.classdefpkg.Options', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
issues = checkcode(unusedImportScope, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.istrue(any(strcmp(ids, 'NLS0030') & lineNumbers == 2));
diagnostics = __code_analyzer__(unusedImportScope, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
unusedImportIndex = find(strcmp(ids, 'NLS0030'), 1);
asserts.istrue(~isempty(unusedImportIndex));
asserts.isequal(diagnostics(unusedImportIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(unusedImportIndex).Fixes) >= 1);
asserts.isequal(diagnostics(unusedImportIndex).Fixes(1).LineStart, 2);
asserts.isequal(diagnostics(unusedImportIndex).Fixes(1).LineEnd, 3);
asserts.isequal(diagnostics(unusedImportIndex).Fixes(1).ReplacementText, '');

literalImportScope = fullfile(fixtureDirectory, 'nelson_lint_literal_import_scope.m');
filewrite(literalImportScope, ['function y = nelson_lint_literal_import_scope()', newline(), ...
  'import(''nelson.classdefpkg.Options'')', newline(), ...
  'y = Options.empty();', newline(), ...
  'Options = y;', newline(), ...
  'end', newline()]);
issues = checkcode(literalImportScope, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.isfalse(any(strcmp(ids, 'NLS0005') & contains({issues.message}, '''Options''')));
asserts.isfalse(any(strcmp(ids, 'NLS0004') & contains({issues.message}, '''Options''')));
asserts.istrue(any(strcmp(ids, 'NLS0029') & lineNumbers == 4));
asserts.isfalse(any(strcmp(ids, 'NLS0030')));

unusedLiteralImportScope = fullfile(fixtureDirectory, 'nelson_lint_unused_literal_import_scope.m');
filewrite(unusedLiteralImportScope, ['function y = nelson_lint_unused_literal_import_scope(x)', newline(), ...
  'import(''nelson.classdefpkg.Options'')', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
issues = checkcode(unusedLiteralImportScope, '-struct', '-id');
ids = {issues.id};
lineNumbers = [issues.line];
asserts.istrue(any(strcmp(ids, 'NLS0030') & lineNumbers == 2));
diagnostics = __code_analyzer__(unusedLiteralImportScope, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
unusedLiteralImportIndex = find(strcmp(ids, 'NLS0030'), 1);
asserts.istrue(~isempty(unusedLiteralImportIndex));
asserts.isequal(diagnostics(unusedLiteralImportIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(unusedLiteralImportIndex).Fixes) >= 1);
asserts.isequal(diagnostics(unusedLiteralImportIndex).Fixes(1).LineStart, 2);
asserts.isequal(diagnostics(unusedLiteralImportIndex).Fixes(1).LineEnd, 3);
asserts.isequal(diagnostics(unusedLiteralImportIndex).Fixes(1).ReplacementText, '');

multiLiteralImportScope = fullfile(fixtureDirectory, 'nelson_lint_multi_literal_import_scope.m');
filewrite(multiLiteralImportScope, ['function y = nelson_lint_multi_literal_import_scope()', newline(), ...
  'import(''nelson.classdefpkg.Options'', ''nelson.classdefpkg.OtherUnused'')', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
issues = checkcode(multiLiteralImportScope, '-struct', '-id');
ids = {issues.id};
asserts.istrue(any(strcmp(ids, 'NLS0030') ...
  & contains({issues.message}, '''nelson.classdefpkg.OtherUnused''')));
asserts.isfalse(any(strcmp(ids, 'NLS0030') & contains({issues.message}, '''Options''')));

literalImportInText = fullfile(fixtureDirectory, 'nelson_lint_literal_import_text.m');
filewrite(literalImportInText, ['function y = nelson_lint_literal_import_text()', newline(), ...
  'msg = ''import(''''nelson.classdefpkg.Options'''')'';', newline(), ...
  'y = 1;', newline(), ...
  'end', newline()]);
issues = checkcode(literalImportInText, '-struct', '-id');
ids = {issues.id};
asserts.isfalse(any(strcmp(ids, 'NLS0030') & contains({issues.message}, '''Options''')));

multiImportScope = fullfile(fixtureDirectory, 'nelson_lint_multi_import_scope.m');
filewrite(multiImportScope, ['function y = nelson_lint_multi_import_scope()', newline(), ...
  'import nelson.classdefpkg.Options, nelson.classdefpkg.OtherUnused', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
diagnostics = __code_analyzer__(multiImportScope, false, '', false, true, false, false);
ids = {diagnostics.CheckID};
multiImportMatches = strcmp(ids, 'NLS0030');
multiImportMatches = multiImportMatches ...
  & contains({diagnostics.Message}, '''nelson.classdefpkg.OtherUnused''');
multiImportIndex = find(multiImportMatches, 1);
asserts.istrue(~isempty(multiImportIndex));
asserts.isequal(diagnostics(multiImportIndex).Fixability, 'safe');
asserts.istrue(numel(diagnostics(multiImportIndex).Fixes) >= 1);
asserts.isequal(diagnostics(multiImportIndex).Fixes(1).LineStart, 2);
asserts.isequal(diagnostics(multiImportIndex).Fixes(1).LineEnd, 2);
asserts.isequal(diagnostics(multiImportIndex).Fixes(1).ReplacementText, '');

wildcardImportScope = fullfile(fixtureDirectory, 'nelson_lint_wildcard_import_scope.m');
filewrite(wildcardImportScope, ['function y = nelson_lint_wildcard_import_scope()', newline(), ...
  'import nelson.classdefpkg.*', newline(), ...
  'y = Options.empty();', newline(), ...
  'Options = y;', newline(), ...
  'end', newline()]);
issues = checkcode(wildcardImportScope, '-struct', '-id');
ids = {issues.id};
asserts.isfalse(any(strcmp(ids, 'NLS0005') & contains({issues.message}, '''Options''')));

%=============================================================================
