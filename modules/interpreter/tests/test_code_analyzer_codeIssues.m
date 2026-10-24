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
folder = tempname();
if ~isdir(folder)
  mkdir(folder);
end
folderCleanup = onCleanup(@() rmdir(folder, 's')); %#ok<NASGU>
source = [folder, '/sample_lint_case.m'];
filewrite(source, ['function y = sample_lint_case(x)', newline(), ...
  'y = x;   ', newline(), ...
  'end']);
directionalSource = [folder, '/directional_lint_case.m'];
fid = fopen(directionalSource, 'wb');
fwrite(fid, uint8([120 32 61 32 49 59 32 226 128 174 10]), 'uint8');
fclose(fid);

ci = codeIssues(folder, 'IncludeSubfolders', true);
asserts.isequal(ci.Files, {folder});
asserts.istrue(istable(ci.Issues));
asserts.istrue(istable(ci.SuppressedIssues));
asserts.istrue(height(ci.Issues) >= 1);
asserts.istrue(any(strcmp({ci.RawDiagnostics.CheckID}, 'NLS0016')));

jsonFile = [folder, '/issues.json'];
export(ci, jsonFile);
asserts.istrue(isfile(jsonFile));
asserts.istrue(contains(fileread(jsonFile), 'NLS0010'));

ci2 = fix(ci);
asserts.istrue(istable(ci2.Issues));
fixedText = fileread(source);
asserts.isfalse(contains(fixedText, [';   ', newline()]));
fid = fopen(directionalSource, 'rb');
fixedBytes = fread(fid, Inf, 'uint8')';
fclose(fid);
asserts.istrue(isempty(strfind(fixedBytes, uint8([226 128 174]))));

unusedImportSource = [folder, '/sample_unused_import_case.m'];
filewrite(unusedImportSource, ['function y = sample_unused_import_case(x)', newline(), ...
  'import nelson.classdefpkg.Options', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
ciImport = codeIssues(unusedImportSource);
asserts.istrue(any(strcmp({ciImport.RawDiagnostics.CheckID}, 'NLS0030')));
ciImportFixed = fix(ciImport);
asserts.istrue(isempty(ciImportFixed.RawDiagnostics));
fixedImportText = fileread(unusedImportSource);
asserts.isfalse(contains(fixedImportText, 'import nelson.classdefpkg.Options'));
asserts.istrue(contains(fixedImportText, ...
  ['function y = sample_unused_import_case(x)', newline(), 'y = x;']));

multiImportSource = [folder, '/sample_multi_import_case.m'];
filewrite(multiImportSource, ['function y = sample_multi_import_case()', newline(), ...
  'import nelson.classdefpkg.Options, nelson.classdefpkg.OtherUnused', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
ciMultiImport = codeIssues(multiImportSource);
multiImportMatches = strcmp({ciMultiImport.RawDiagnostics.CheckID}, 'NLS0030');
multiImportMessages = contains({ciMultiImport.RawDiagnostics.Message}, ...
  '''nelson.classdefpkg.OtherUnused''');
multiImportMatches = multiImportMatches & multiImportMessages;
asserts.istrue(any(multiImportMatches));
ciMultiImportFixed = fix(ciMultiImport);
asserts.istrue(isempty(ciMultiImportFixed.RawDiagnostics));
fixedMultiImportText = fileread(multiImportSource);
asserts.istrue(contains(fixedMultiImportText, 'import nelson.classdefpkg.Options'));
asserts.isfalse(contains(fixedMultiImportText, 'OtherUnused'));

indentedMultiImportSource = [folder, '/sample_indented_multi_import_case.m'];
filewrite(indentedMultiImportSource, ['function y = sample_indented_multi_import_case()', newline(), ...
  '  import nelson.classdefpkg.Options, nelson.classdefpkg.OtherUnused', newline(), ...
  '  y = Options.empty();', newline(), ...
  'end', newline()]);
ciIndentedMultiImport = codeIssues(indentedMultiImportSource);
indentedMultiImportMatches = strcmp({ciIndentedMultiImport.RawDiagnostics.CheckID}, 'NLS0030');
indentedMultiImportMessages ...
  = contains({ciIndentedMultiImport.RawDiagnostics.Message}, ...
  '''nelson.classdefpkg.OtherUnused''');
asserts.istrue(any(indentedMultiImportMatches & indentedMultiImportMessages));
ciIndentedMultiImportFixed = fix(ciIndentedMultiImport);
asserts.istrue(isempty(ciIndentedMultiImportFixed.RawDiagnostics));
fixedIndentedMultiImportText = fileread(indentedMultiImportSource);
asserts.istrue(contains(fixedIndentedMultiImportText, '  import nelson.classdefpkg.Options'));
asserts.isfalse(contains(fixedIndentedMultiImportText, 'OtherUnused'));

continuedImportSource = [folder, '/sample_continued_import_case.m'];
filewrite(continuedImportSource, ['function y = sample_continued_import_case()', newline(), ...
  'import nelson.classdefpkg.Options, ...', newline(), ...
  '  nelson.classdefpkg.OtherUnused', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
ciContinuedImport = codeIssues(continuedImportSource);
continuedImportMatches = strcmp({ciContinuedImport.RawDiagnostics.CheckID}, 'NLS0030');
continuedImportMessages = contains({ciContinuedImport.RawDiagnostics.Message}, ...
  '''nelson.classdefpkg.OtherUnused''');
asserts.istrue(any(continuedImportMatches & continuedImportMessages));
ciContinuedImportFixed = fix(ciContinuedImport);
asserts.istrue(isempty(ciContinuedImportFixed.RawDiagnostics));
fixedContinuedImportText = fileread(continuedImportSource);
asserts.istrue(contains(fixedContinuedImportText, 'import nelson.classdefpkg.Options'));
asserts.isfalse(contains(fixedContinuedImportText, 'OtherUnused'));
asserts.isfalse(contains(fixedContinuedImportText, '...'));

continuedImportCommentSource = [folder, '/sample_continued_import_comment_case.m'];
filewrite(continuedImportCommentSource, ...
  ['function y = sample_continued_import_comment_case()', newline(), ...
  'import nelson.classdefpkg.Options, ... % keep note', newline(), ...
  '  nelson.classdefpkg.OtherUnused', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
ciContinuedImportComment = codeIssues(continuedImportCommentSource);
continuedImportCommentMatches ...
  = strcmp({ciContinuedImportComment.RawDiagnostics.CheckID}, 'NLS0030');
continuedImportCommentMessages ...
  = contains({ciContinuedImportComment.RawDiagnostics.Message}, ...
  '''nelson.classdefpkg.OtherUnused''');
asserts.istrue(any(continuedImportCommentMatches & continuedImportCommentMessages));
ciContinuedImportCommentFixed = fix(ciContinuedImportComment);
asserts.istrue(isempty(ciContinuedImportCommentFixed.RawDiagnostics));
fixedContinuedImportCommentText = fileread(continuedImportCommentSource);
asserts.istrue(contains(fixedContinuedImportCommentText, ...
  'import nelson.classdefpkg.Options % keep note'));
asserts.isfalse(contains(fixedContinuedImportCommentText, 'OtherUnused'));
asserts.isfalse(contains(fixedContinuedImportCommentText, '...'));

doubleQuotedImportSource = [folder, '/sample_double_quoted_import_case.m'];
filewrite(doubleQuotedImportSource, ['function y = sample_double_quoted_import_case(x)', newline(), ...
  'import("nelson.classdefpkg.OtherUnused")', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
ciDoubleQuotedImport = codeIssues(doubleQuotedImportSource);
doubleQuotedImportMatches = strcmp({ciDoubleQuotedImport.RawDiagnostics.CheckID}, 'NLS0030');
doubleQuotedImportMessages ...
  = contains({ciDoubleQuotedImport.RawDiagnostics.Message}, ...
  '''nelson.classdefpkg.OtherUnused''');
asserts.istrue(any(doubleQuotedImportMatches & doubleQuotedImportMessages));
ciDoubleQuotedImportFixed = fix(ciDoubleQuotedImport);
asserts.istrue(isempty(ciDoubleQuotedImportFixed.RawDiagnostics));
fixedDoubleQuotedImportText = fileread(doubleQuotedImportSource);
asserts.isfalse(contains(fixedDoubleQuotedImportText, 'OtherUnused'));
asserts.istrue(contains(fixedDoubleQuotedImportText, ...
  ['function y = sample_double_quoted_import_case(x)', newline(), 'y = x;']));

multiDoubleQuotedImportSource = [folder, '/sample_multi_double_quoted_import_case.m'];
filewrite(multiDoubleQuotedImportSource, ...
  ['function y = sample_multi_double_quoted_import_case()', newline(), ...
  'import("nelson.classdefpkg.Options", "nelson.classdefpkg.OtherUnused")', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
ciMultiDoubleQuotedImport = codeIssues(multiDoubleQuotedImportSource);
multiDoubleQuotedImportMatches ...
  = strcmp({ciMultiDoubleQuotedImport.RawDiagnostics.CheckID}, 'NLS0030');
multiDoubleQuotedImportMessages ...
  = contains({ciMultiDoubleQuotedImport.RawDiagnostics.Message}, ...
  '''nelson.classdefpkg.OtherUnused''');
asserts.istrue(any(multiDoubleQuotedImportMatches & multiDoubleQuotedImportMessages));
ciMultiDoubleQuotedImportFixed = fix(ciMultiDoubleQuotedImport);
asserts.istrue(isempty(ciMultiDoubleQuotedImportFixed.RawDiagnostics));
fixedMultiDoubleQuotedImportText = fileread(multiDoubleQuotedImportSource);
asserts.istrue(contains(fixedMultiDoubleQuotedImportText, '"nelson.classdefpkg.Options"'));
asserts.isfalse(contains(fixedMultiDoubleQuotedImportText, 'OtherUnused'));

continuedDoubleQuotedImportSource = [folder, '/sample_continued_double_quoted_import_case.m'];
filewrite(continuedDoubleQuotedImportSource, ...
  ['function y = sample_continued_double_quoted_import_case()', newline(), ...
  'import("nelson.classdefpkg.Options", ...', newline(), ...
  '  "nelson.classdefpkg.OtherUnused")', newline(), ...
  'y = Options.empty();', newline(), ...
  'end', newline()]);
ciContinuedDoubleQuotedImport = codeIssues(continuedDoubleQuotedImportSource);
continuedDoubleQuotedImportMatches ...
  = strcmp({ciContinuedDoubleQuotedImport.RawDiagnostics.CheckID}, 'NLS0030');
continuedDoubleQuotedImportMessages ...
  = contains({ciContinuedDoubleQuotedImport.RawDiagnostics.Message}, ...
  '''nelson.classdefpkg.OtherUnused''');
asserts.istrue(any(continuedDoubleQuotedImportMatches & continuedDoubleQuotedImportMessages));
ciContinuedDoubleQuotedImportFixed = fix(ciContinuedDoubleQuotedImport);
asserts.istrue(isempty(ciContinuedDoubleQuotedImportFixed.RawDiagnostics));
fixedContinuedDoubleQuotedImportText = fileread(continuedDoubleQuotedImportSource);
asserts.istrue(contains(fixedContinuedDoubleQuotedImportText, '"nelson.classdefpkg.Options"'));
asserts.isfalse(contains(fixedContinuedDoubleQuotedImportText, 'OtherUnused'));
asserts.isfalse(contains(fixedContinuedDoubleQuotedImportText, '...'));

unusedAssignmentSource = [folder, '/sample_unused_assignment_case.m'];
filewrite(unusedAssignmentSource, ['function y = sample_unused_assignment_case(x)', newline(), ...
  'tmp = 1;', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
ciUnusedAssignment = codeIssues(unusedAssignmentSource);
unusedAssignmentMatches = strcmp({ciUnusedAssignment.RawDiagnostics.CheckID}, 'NLS0004');
unusedAssignmentMatches = unusedAssignmentMatches ...
  & contains({ciUnusedAssignment.RawDiagnostics.Message}, '''tmp''');
asserts.istrue(any(unusedAssignmentMatches));
ciUnusedAssignmentFixed = fix(ciUnusedAssignment);
asserts.istrue(isempty(ciUnusedAssignmentFixed.RawDiagnostics));
fixedUnusedAssignmentText = fileread(unusedAssignmentSource);
asserts.isfalse(contains(fixedUnusedAssignmentText, 'tmp = 1;'));
asserts.istrue(contains(fixedUnusedAssignmentText, 'y = x;'));

overlappingFixSource = [folder, '/sample_overlapping_fix_case.m'];
filewrite(overlappingFixSource, ['function y = sample_overlapping_fix_case(x)', newline(), ...
  'tmp = 1;   ', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
ciOverlappingFix = codeIssues(overlappingFixSource);
overlappingIds = {ciOverlappingFix.RawDiagnostics.CheckID};
asserts.istrue(any(strcmp(overlappingIds, 'NLS0004')));
asserts.istrue(any(strcmp(overlappingIds, 'NLS0010')));
ciOverlappingFixFixed = fix(ciOverlappingFix);
asserts.istrue(isempty(ciOverlappingFixFixed.RawDiagnostics));
fixedOverlappingFixText = fileread(overlappingFixSource);
asserts.isfalse(contains(fixedOverlappingFixText, 'tmp = 1;'));
asserts.istrue(contains(fixedOverlappingFixText, ...
  ['function y = sample_overlapping_fix_case(x)', newline(), 'y = x;']));

unreachableSource = [folder, '/sample_unreachable_case.m'];
filewrite(unreachableSource, ['function y = sample_unreachable_case(x)', newline(), ...
  'y = x;', newline(), ...
  'return', newline(), ...
  'z = 1;', newline(), ...
  'end', newline()]);
ciUnreachable = codeIssues(unreachableSource);
unreachableMatches = strcmp({ciUnreachable.RawDiagnostics.CheckID}, 'NLS0003');
asserts.istrue(any(unreachableMatches));
unreachableDiagnostics = ciUnreachable.RawDiagnostics(unreachableMatches);
asserts.isfalse(isempty(unreachableDiagnostics(1).Fixes));
ciUnreachableFixed = fix(ciUnreachable);
asserts.istrue(isempty(ciUnreachableFixed.RawDiagnostics));
fixedUnreachableText = fileread(unreachableSource);
asserts.isfalse(contains(fixedUnreachableText, 'z = 1;'));
asserts.istrue(contains(fixedUnreachableText, 'return'));

unusedLocalSource = [folder, '/sample_unused_local_function_case.m'];
filewrite(unusedLocalSource, ['function y = sample_unused_local_function_case(x)', newline(), ...
  'y = helper(x);', newline(), ...
  'end', newline(), ...
  'function y = helper(x)', newline(), ...
  'y = x + 1;', newline(), ...
  'end', newline(), ...
  'function y = unused_helper(x)', newline(), ...
  'y = x - 1;', newline(), ...
  'end', newline()]);
ciUnusedLocal = codeIssues(unusedLocalSource);
asserts.istrue(any(strcmp({ciUnusedLocal.RawDiagnostics.CheckID}, 'NLS0023')));
ciUnusedLocalFixed = fix(ciUnusedLocal);
asserts.istrue(isempty(ciUnusedLocalFixed.RawDiagnostics));
fixedUnusedLocalText = fileread(unusedLocalSource);
asserts.istrue(contains(fixedUnusedLocalText, 'function y = helper(x)'));
asserts.isfalse(contains(fixedUnusedLocalText, 'unused_helper'));

classNameSource = [folder, '/SampleClassNameCase.m'];
filewrite(classNameSource, ['classdef WrongSampleClassName', newline(), ...
  'end', newline()]);
ciClassName = codeIssues(classNameSource);
classNameMatches = strcmp({ciClassName.RawDiagnostics.CheckID}, 'NLS0002');
asserts.istrue(any(classNameMatches));
classNameDiagnostics = ciClassName.RawDiagnostics(classNameMatches);
asserts.isequal(classNameDiagnostics(1).Fixability, 'safe');
asserts.isequal(classNameDiagnostics(1).Fixes(1).ReplacementText, ...
  'SampleClassNameCase');
ciClassNameFixed = fix(ciClassName);
asserts.istrue(isempty(ciClassNameFixed.RawDiagnostics));
fixedClassNameText = fileread(classNameSource);
asserts.istrue(contains(fixedClassNameText, 'classdef SampleClassNameCase'));
asserts.isfalse(contains(fixedClassNameText, 'WrongSampleClassName'));

unsafeLoadSource = [folder, '/sample_unsafe_load_case.m'];
filewrite(unsafeLoadSource, ['function y = sample_unsafe_load_case()', newline(), ...
  'load workspace_data.mat', newline(), ...
  'y = 1;', newline(), ...
  'end', newline()]);
ciUnsafeLoad = codeIssues(unsafeLoadSource);
unsafeLoadMatches = strcmp({ciUnsafeLoad.RawDiagnostics.CheckID}, 'NLS0022');
unsafeLoadMatches = unsafeLoadMatches ...
  & contains({ciUnsafeLoad.RawDiagnostics.Message}, '''load''');
asserts.istrue(any(unsafeLoadMatches));
unsafeLoadIndex = find(unsafeLoadMatches, 1);
asserts.isequal(ciUnsafeLoad.RawDiagnostics(unsafeLoadIndex).Fixability, 'unsafe');
asserts.isfalse(ciUnsafeLoad.RawDiagnostics(unsafeLoadIndex).Fixes(1).Safe);
ciUnsafeLoadFixed = fix(ciUnsafeLoad);
asserts.istrue(any(strcmp({ciUnsafeLoadFixed.RawDiagnostics.CheckID}, 'NLS0022')));
fixedUnsafeLoadText = fileread(unsafeLoadSource);
asserts.istrue(contains(fixedUnsafeLoadText, 'load workspace_data.mat'));
asserts.isfalse(contains(fixedUnsafeLoadText, 'loadedData = load'));

docSource = [folder, '/sample_doc_case.m'];
filewrite(docSource, ['function y = sample_doc_case(x)', newline(), ...
  'y = x;', newline(), ...
  'end', newline()]);
configDocs = [folder, '/docs_config.json'];
filewrite(configDocs, ['{', newline(), ...
  '  "version": 2,', newline(), ...
  '  "rules": { "NLS0027": { "level": "warning" } }', newline(), ...
  '}']);
ciDocs = codeIssues(docSource, 'CodeAnalyzerConfiguration', configDocs);
asserts.istrue(any(strcmp({ciDocs.RawDiagnostics.CheckID}, 'NLS0027')));
ciDocsFixed = fix(ciDocs);
asserts.istrue(isempty(ciDocsFixed.RawDiagnostics));
fixedDocText = fileread(docSource);
asserts.istrue(contains(fixedDocText, '% sample_doc_case - Add help text.'));

classDocSource = [folder, '/SampleDocClass.m'];
filewrite(classDocSource, ['classdef SampleDocClass', newline(), ...
  '  properties', newline(), ...
  '    Value', newline(), ...
  '  end', newline(), ...
  'end', newline()]);
ciClassDocs = codeIssues(classDocSource, 'CodeAnalyzerConfiguration', configDocs);
classDocMatches = strcmp({ciClassDocs.RawDiagnostics.CheckID}, 'NLS0027');
classDocMatches = classDocMatches ...
  & contains({ciClassDocs.RawDiagnostics.Message}, 'Class ''SampleDocClass''');
asserts.istrue(any(classDocMatches));
ciClassDocsFixed = fix(ciClassDocs);
asserts.istrue(isempty(ciClassDocsFixed.RawDiagnostics));
fixedClassDocText = fileread(classDocSource);
asserts.istrue(contains(fixedClassDocText, '% SampleDocClass - Add help text.'));
%=============================================================================
