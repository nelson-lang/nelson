%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
previousPreferencesPath = getenv('NELSON_PREFERENCES_PATH');
preferencesRoot = [tempdir(), 'preferences_uigetpref_', createGUID()];
mkdir(preferencesRoot, 's');
cleanupPreferencesPath = onCleanup(@() setenv('NELSON_PREFERENCES_PATH', previousPreferencesPath)); %#ok<NLS0004>
setenv('NELSON_PREFERENCES_PATH', preferencesRoot);
%=============================================================================
group = ['uigetpref_fixes_', strrep(createGUID(), '-', '_')];
if ispref(group)
  rmpref(group);
end
%=============================================================================
% A saved answer is returned in lower case, the stored value is kept.
setpref(group, 'saved', 'No');
[pval, tf] = uigetpref(group, 'saved', 'Title', 'Question', {'Yes', 'No'});
asserts.isequal(pval, 'no');
asserts.isfalse(tf);
asserts.isequal(getpref(group, 'saved'), 'No');
%=============================================================================
% A string scalar answer comes back as a lower case character vector.
setpref(group, 'savedString', "Hello");
asserts.isequal(uigetpref(group, 'savedString', 'Title', 'Question', {'Yes', 'No'}), 'hello');
%=============================================================================
% A non-text answer is returned as is.
setpref(group, 'savedNumber', 3);
[pval, tf] = uigetpref(group, 'savedNumber', 'Title', 'Question', {'Yes', 'No'});
asserts.isequal(pval, 3);
asserts.isfalse(tf);
%=============================================================================
% Only the exact text 'ask' shows the dialog again.
setpref(group, 'upperAsk', 'ASK');
[pval, tf] = uigetpref(group, 'upperAsk', 'Title', 'Question', {'Yes', 'No'});
asserts.isequal(pval, 'ask');
asserts.isfalse(tf);
%=============================================================================
% Every documented choice form and option is accepted with a saved answer.
helpCalled = false; %#ok<NLS0004>
asserts.isequal(uigetpref(group, 'saved', 'T', 'Q|Line 2', {'yes', 'no'; 'Yes', 'No'}), 'no');
asserts.isequal(uigetpref(group, 'saved', 'T', {'Q', 'Line 2'}, 'Yes|No'), 'no');
asserts.isequal(uigetpref(group, 'saved', "T", ["Q", "Line 2"], ["yes", "no"; "Yes", "No"]), 'no');
asserts.isequal(uigetpref(group, 'saved', 'T', 'Q', {'Yes', 'No'}, ...
  'CheckboxState', 1, 'CheckboxString', 'Remember', 'ExtraOptions', {'Cancel'}, ...
  'DefaultButton', 'No', 'HelpString', 'Help', 'HelpFcn', @() disp('help'), ...
  'HelpTopic', 'uigetpref', 'DialogProps', struct('Color', 'w')), 'no');
asserts.isequal(uigetpref(group, 'saved', 'T', 'Q', {'Yes', 'No'}, ...
  'checkboxstate', 'off', 'helpfcn', 'disp(1)', 'extraoptions', 'Cancel'), 'no');
asserts.isequal(uigetpref(group, 'saved', 'T', 'Q', {'Yes', 'No'}, ...
  'HelpFcn', {@(x) disp(x), 'help'}), 'no');
%=============================================================================
% Name-value pairs are checked even when a saved answer exists.
msg = _('Unknown preference dialog option.');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', {''Yes'', ''No''}, ''Bogus'', 1)', ...
  msg, 'Nelson:uigetpref:unknownOption');
msg = _('Name-value arguments must be paired.');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', {''Yes'', ''No''}, ''CheckboxState'')', ...
  msg, 'Nelson:preferences:nameValueNotPaired');
msg = _('CheckboxState must be a logical scalar, ''on'' or ''off''.');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', {''Yes'', ''No''}, ''CheckboxState'', ''maybe'')', ...
  msg, 'Nelson:uigetpref:invalidCheckboxState');
msg = _('HelpFcn must be text, a function handle or a cell array whose first element is a function handle.');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', {''Yes'', ''No''}, ''HelpFcn'', 3)', ...
  msg, 'Nelson:uigetpref:invalidHelpFcn');
msg = _('DialogProps must be a struct.');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', {''Yes'', ''No''}, ''DialogProps'', 1)', ...
  msg, 'Nelson:uigetpref:invalidDialogProps');
msg = sprintf(_('%s must be a character vector or a string scalar.'), 'HelpString');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', {''Yes'', ''No''}, ''HelpString'', 1)', ...
  msg, 'Nelson:uigetpref:textExpected');
%=============================================================================
% Positional inputs are checked even when a saved answer exists.
msg = sprintf(_('%s must be a character vector or a string scalar.'), 'title');
asserts.checkerror('uigetpref(group, ''saved'', 1, ''Q'', {''Yes'', ''No''})', ...
  msg, 'Nelson:uigetpref:textExpected');
msg = sprintf(_('%s must be text, a string array or a cell array of character vectors.'), 'question');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', 1, {''Yes'', ''No''})', ...
  msg, 'Nelson:uigetpref:textListExpected');
msg = sprintf(_('%s must be text, a string array or a cell array of character vectors.'), 'pref_choices');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', 1)', ...
  msg, 'Nelson:uigetpref:textListExpected');
msg = _('At least one preference button expected.');
asserts.checkerror('uigetpref(group, ''saved'', ''T'', ''Q'', {})', ...
  msg, 'Nelson:uigetpref:buttonExpected');
asserts.isequal(getpref(group, 'saved'), 'No');
%=============================================================================
% An invalid call leaves a missing preference untouched.
asserts.checkerror('uigetpref(group, ''missing'', ''T'', ''Q'', {''Yes'', ''No''}, ''Bogus'', 1)', ...
  _('Unknown preference dialog option.'), 'Nelson:uigetpref:unknownOption');
asserts.isfalse(ispref(group, 'missing'));
%=============================================================================
rmpref(group);
asserts.isfalse(ispref(group));
if isdir(preferencesRoot)
  rmdir(preferencesRoot, 's');
end
%=============================================================================
