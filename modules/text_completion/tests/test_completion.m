%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
asserts.isequal(exist('completion', 'builtin'), 5);
%=============================================================================
completionPoint = ClassdefPoint(1, 2);
completionPointResult = completion('completionPoint.');
asserts.istrue(completionPointResult.showpopup);
asserts.isequal(completionPointResult.prefix, '');
asserts.istrue(any(strcmp(completionPointResult.properties, 'X')));
asserts.istrue(any(strcmp(completionPointResult.methods, 'magnitude')));
asserts.istrue(isempty(completionPointResult.builtin));
asserts.istrue(isempty(completionPointResult.macros));
asserts.istrue(isempty(completionPointResult.variables));
%=============================================================================
completionPointMethodResult = completion('completionPoint.m');
asserts.istrue(completionPointMethodResult.showpopup);
asserts.isequal(completionPointMethodResult.prefix, 'm');
asserts.istrue(any(strcmp(completionPointMethodResult.methods, 'magnitude')));
%=============================================================================
completionPoints = [ClassdefPoint(3, 4), ClassdefPoint(5, 6)];
completionPointsResult = completion('completionPoints.');
asserts.istrue(completionPointsResult.showpopup);
asserts.istrue(any(strcmp(completionPointsResult.properties, 'Label')));
asserts.istrue(any(strcmp(completionPointsResult.methods, 'shift')));
%=============================================================================
classCompletionResult = completion('ClassdefPoint.');
asserts.istrue(classCompletionResult.showpopup);
asserts.istrue(any(strcmp(classCompletionResult.properties, 'Dimension')));
asserts.istrue(any(strcmp(classCompletionResult.methods, 'origin')));
%=============================================================================
hiddenCompletionDir = [tempname(), '/'];
if isdir(hiddenCompletionDir)
  rmdir(hiddenCompletionDir, 's');
end
mkdir(hiddenCompletionDir);
hiddenCompletionFile = [hiddenCompletionDir, 'NelsonHiddenCompletionClass.m'];
filewrite(hiddenCompletionFile, ["classdef (Hidden) NelsonHiddenCompletionClass"; ...
                                 "  properties"; ...
                                 "    Value = 1"; ...
                                 "  end"; ...
                                 "end"]);
addpath(hiddenCompletionDir, '-begin');
hiddenClassResult = completion('NelsonHiddenCompletion');
asserts.isfalse(any(strcmp(hiddenClassResult.macros, 'NelsonHiddenCompletionClass')));
rmpath(hiddenCompletionDir);
rmdir(hiddenCompletionDir, 's');
%=============================================================================
classdefNameResult = completion('Classdef');
asserts.isfalse(any(startsWith(classdefNameResult.macros, '@Classdef')));
%=============================================================================
internalBuiltinResult = completion('__');
asserts.isfalse(any(startsWith(internalBuiltinResult.builtin, '__')));
%=============================================================================
% Member completion contract: prefix is the leaf suffix of the line and every
% entry replaces exactly that suffix (no container duplication on insert).
dottedBuiltinResult = completion('asserts.');
asserts.istrue(dottedBuiltinResult.showpopup);
asserts.isequal(dottedBuiltinResult.prefix, '');
asserts.istrue(any(strcmp(dottedBuiltinResult.builtin, 'istrue')));
asserts.istrue(any(strcmp(dottedBuiltinResult.builtin, 'fields')));
asserts.isfalse(any(startsWith(dottedBuiltinResult.builtin, '__')));
%=============================================================================
dottedBuiltinPartialResult = completion('asserts.i');
asserts.istrue(dottedBuiltinPartialResult.showpopup);
asserts.isequal(dottedBuiltinPartialResult.prefix, 'i');
asserts.istrue(any(strcmp(dottedBuiltinPartialResult.builtin, 'istrue')));
asserts.istrue(any(strcmp(dottedBuiltinPartialResult.builtin, 'isfalse')));
asserts.isfalse(any(strcmp(dottedBuiltinPartialResult.builtin, 'asserts.istrue')));
%=============================================================================
nelsonPackageResult = completion('nelson.');
asserts.istrue(nelsonPackageResult.showpopup);
asserts.isequal(nelsonPackageResult.prefix, '');
asserts.istrue(any(strcmp(nelsonPackageResult.macros, 'lang')));
%=============================================================================
nelsonPackagePartialResult = completion('nelson.l');
asserts.istrue(nelsonPackagePartialResult.showpopup);
asserts.isequal(nelsonPackagePartialResult.prefix, 'l');
asserts.istrue(any(strcmp(nelsonPackagePartialResult.macros, 'lang')));
asserts.isfalse(any(strcmp(nelsonPackagePartialResult.macros, 'nelson.lang')));
%=============================================================================
nelsonLangResult = completion('nelson.lang.');
asserts.istrue(nelsonLangResult.showpopup);
asserts.isequal(nelsonLangResult.prefix, '');
asserts.istrue(any(strcmp(nelsonLangResult.macros, 'makeValidName')));
%=============================================================================
% Member completion also works when the dotted symbol is not at line start.
midLineMemberResult = completion('x = nelson.l');
asserts.istrue(midLineMemberResult.showpopup);
asserts.isequal(midLineMemberResult.prefix, 'l');
asserts.istrue(any(strcmp(midLineMemberResult.macros, 'lang')));
%=============================================================================
callMemberResult = completion('plot(nelson.l');
asserts.istrue(callMemberResult.showpopup);
asserts.isequal(callMemberResult.prefix, 'l');
asserts.istrue(any(strcmp(callMemberResult.macros, 'lang')));
%=============================================================================
completionCounter = ClassdefCounter(7);
completionCounterResult = completion('completionCounter.');
asserts.istrue(completionCounterResult.showpopup);
asserts.istrue(any(strcmp(completionCounterResult.properties, 'Count')));
asserts.istrue(any(strcmp(completionCounterResult.methods, 'increment')));
asserts.istrue(any(strcmp(completionCounterResult.methods, 'zero')));
delete(completionCounter);
%=============================================================================
completionSaveFile = [tempname(), '.mat'];
completionSavedCounters = [ClassdefCounter(8), ClassdefCounter(9)];
save(completionSaveFile, 'completionSavedCounters');
delete(completionSavedCounters);
clear completionSavedCounters;
clear classes;
load(completionSaveFile);
restoredCounterResult = completion('completionSavedCounters.');
asserts.istrue(restoredCounterResult.showpopup);
asserts.istrue(any(strcmp(restoredCounterResult.properties, 'Count')));
asserts.istrue(any(strcmp(restoredCounterResult.methods, 'increment')));
delete(completionSavedCounters);
rmfile(completionSaveFile);
%=============================================================================
deletedCounter = ClassdefCounter(10);
delete(deletedCounter);
deletedCounterResult = completion('deletedCounter.');
asserts.isfalse(deletedCounterResult.showpopup);
asserts.istrue(isempty(deletedCounterResult.properties));
asserts.istrue(isempty(deletedCounterResult.methods));
%=============================================================================
completionPayload.Figures = {struct('Properties', 1)};
completionPayloadResult = completion('completionPayload.Figures{1}.Prop');
asserts.istrue(completionPayloadResult.showpopup);
asserts.isequal(completionPayloadResult.prefix, 'Prop');
asserts.istrue(any(strcmp(completionPayloadResult.fields, 'Properties')));
asserts.istrue(isempty(completionPayloadResult.builtin));
asserts.istrue(isempty(completionPayloadResult.macros));
asserts.istrue(isempty(completionPayloadResult.variables));
clear completionPayload;
%=============================================================================
completion_rank_exact = 1;
completion_rank_exact_long = 2;
other_completion_rank_exact = 3;
completionRankResult = completion('completion_rank_exact');
asserts.istrue(completionRankResult.showpopup);
asserts.isequal(completionRankResult.variables{1}, 'completion_rank_exact');
asserts.istrue(any(strcmp(completionRankResult.variables, 'completion_rank_exact_long')));
asserts.isfalse(any(strcmp(completionRankResult.variables, 'other_completion_rank_exact')));
clear completion_rank_exact completion_rank_exact_long other_completion_rank_exact;
%=============================================================================
prefixOnlyResult = completion('ab');
asserts.istrue(prefixOnlyResult.showpopup);
asserts.istrue(any(strcmp(prefixOnlyResult.builtin, 'abs')));
asserts.istrue(any(strcmp(prefixOnlyResult.builtin, 'netcdf.abort')));
asserts.isfalse(any(strcmp(prefixOnlyResult.macros, 'table')));
asserts.isfalse(any(strcmp(prefixOnlyResult.macros, 'bwlabel')));
asserts.isfalse(any(strcmp(prefixOnlyResult.macros, 'labelmatrix')));
asserts.isfalse(any(strcmp(prefixOnlyResult.macros, 'clabel')));
%=============================================================================
uip_completion_first = 1;
uip_completion_second = 2;
uipCompletionResult = completion('uip');
asserts.istrue(uipCompletionResult.showpopup);
asserts.isequal(uipCompletionResult.prefix, 'uip');
asserts.istrue(any(strcmp(uipCompletionResult.variables, 'uip_completion_first')));
asserts.istrue(any(strcmp(uipCompletionResult.variables, 'uip_completion_second')));
clear uip_completion_first uip_completion_second;
%=============================================================================
load = 1;
loadCompletionResult = completion('load');
asserts.istrue(any(strcmp(loadCompletionResult.variables, 'load')));
asserts.isfalse(any(strcmp(loadCompletionResult.builtin, 'load')));
asserts.isfalse(any(strcmp(loadCompletionResult.macros, 'load')));
clear load;
%=============================================================================
editMacroResult = completion('edit test_');
asserts.istrue(editMacroResult.showpopup);
asserts.isequal(editMacroResult.prefix, 'test_');
asserts.istrue(any(strcmp(editMacroResult.macros, 'test_run')));
%=============================================================================
% Matching is case-insensitive: the entry replaces the typed token and
% corrects its case on insert.
caseInsensitiveResult = completion('DIS');
asserts.istrue(caseInsensitiveResult.showpopup);
asserts.istrue(any(strcmp(caseInsensitiveResult.builtin, 'disp')));
caseMacroResult = completion('TEST_RU');
asserts.istrue(caseMacroResult.showpopup);
asserts.istrue(any(strcmp(caseMacroResult.macros, 'test_run')));
caseMemberResult = completion('nelson.L');
asserts.istrue(caseMemberResult.showpopup);
asserts.istrue(any(strcmp(caseMemberResult.macros, 'lang')));
%=============================================================================
% Language keywords are proposed with builtins.
keywordResult = completion('functi');
asserts.istrue(keywordResult.showpopup);
asserts.istrue(any(strcmp(keywordResult.builtin, 'function')));
keywordWhileResult = completion('whil');
asserts.istrue(any(strcmp(keywordWhileResult.builtin, 'while')));
%=============================================================================
% Functions living in the current directory are proposed like path functions.
completionCwdDir = [tempname(), '/'];
if isdir(completionCwdDir)
  rmdir(completionCwdDir, 's');
end
mkdir(completionCwdDir);
filewrite([completionCwdDir, 'completion_cwd_function.m'], 'disp(1)');
completionCwdCurrentDir = pwd();
completionCwdCleanup = onCleanup(@() cd(completionCwdCurrentDir));
cd(completionCwdDir);
cwdMacroResult = completion('completion_cwd_f');
asserts.istrue(cwdMacroResult.showpopup);
asserts.istrue(any(strcmp(cwdMacroResult.macros, 'completion_cwd_function')));
cd(completionCwdCurrentDir);
clear completionCwdCleanup;
rmdir(completionCwdDir, 's');
%=============================================================================
completionRankingDir = [tempname(), '/'];
if isdir(completionRankingDir)
  rmdir(completionRankingDir, 's');
end
mkdir(completionRankingDir);
filewrite([completionRankingDir, 'completion_rank_file.txt'], 'Nelson');
completionRankingCurrentDir = pwd();
completionRankingCleanup = onCleanup(@() cd(completionRankingCurrentDir));
cd(completionRankingDir);
bareFileResult = completion('completion_rank_');
asserts.istrue(isempty(bareFileResult.files));
commandFileResult = completion('cd completion_rank_');
asserts.istrue(commandFileResult.showpopup);
asserts.istrue(any(strcmp(commandFileResult.files, 'completion_rank_file.txt')));
asserts.istrue(isempty(commandFileResult.macros));
pathFileResult = completion([completionRankingDir, 'completion_rank_']);
asserts.istrue(pathFileResult.showpopup);
asserts.istrue(any(strcmp(pathFileResult.files, 'completion_rank_file.txt')));
cd(completionRankingCurrentDir);
clear completionRankingCleanup;
rmdir(completionRankingDir, 's');
%=============================================================================
rmpath(path_test);
%=============================================================================
