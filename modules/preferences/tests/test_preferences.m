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
preferencesRoot = [tempdir(), 'preferences_', createGUID()];
mkdir(preferencesRoot, 's');
cleanupPreferencesPath = onCleanup(@() setenv('NELSON_PREFERENCES_PATH', previousPreferencesPath)); %#ok<NASGU>
setenv('NELSON_PREFERENCES_PATH', preferencesRoot);
%=============================================================================
asserts.isequal(nargin('getpref'), 3);
asserts.isequal(nargout('getpref'), 1);
asserts.isequal(nargin('setpref'), 3);
asserts.isequal(nargout('setpref'), 0);
asserts.isequal(nargin('addpref'), 3);
asserts.isequal(nargout('addpref'), 0);
asserts.isequal(nargin('ispref'), 2);
asserts.isequal(nargout('ispref'), 1);
asserts.isequal(nargin('rmpref'), 2);
asserts.isequal(nargout('rmpref'), 0);
asserts.isequal(nargin('uigetpref'), -5);
asserts.isequal(nargout('uigetpref'), 2);
asserts.isequal(nargin('uisetpref'), 1);
asserts.isequal(nargout('uisetpref'), 1);
asserts.isequal(nargin('preferences'), 1);
asserts.isequal(nargout('preferences'), 0);
%=============================================================================
group = 'nelson_pref_test';
if ispref(group)
  rmpref(group);
end
%=============================================================================
asserts.isfalse(ispref(group));
addpref(group, 'alpha', 1);
asserts.istrue(ispref(group));
asserts.istrue(ispref(group, 'alpha'));
asserts.isequal(getpref(group, 'alpha'), 1);
%=============================================================================
setpref(group, 'alpha', uint8([1 2 3]));
asserts.isequal(getpref(group, 'alpha'), uint8([1 2 3]));
%=============================================================================
defaults = {42, 'two'};
values = getpref(group, {'beta', 'gamma'}, defaults);
asserts.isequal(values, defaults);
asserts.isequal(getpref(group, {'alpha', 'beta'}), {uint8([1 2 3]), 42});
asserts.isequal(ispref(group, {'alpha', 'missing', 'gamma'}), logical([1 0 1]));
%=============================================================================
setpref(group, ["delta", "epsilon"], {struct('x', 7), single([1 2])});
allPrefs = getpref();
asserts.istrue(isfield(allPrefs, group));
asserts.isequal(allPrefs.(group).delta.x, 7);
asserts.isequal(allPrefs.(group).epsilon, single([1 2]));
%=============================================================================
rmpref(group, {'beta', 'gamma'});
asserts.isequal(ispref(group, {'beta', 'gamma'}), logical([0 0]));
asserts.istrue(ispref(group));
%=============================================================================
setpref(group, 'promptChoice', 'no');
[pval, tf] = uigetpref(group, 'promptChoice', 'Title', 'Question', {'Yes', 'No'});
asserts.isequal(pval, 'no');
asserts.isfalse(tf);
%=============================================================================
rmpref(group);
asserts.isfalse(ispref(group));
if isdir(preferencesRoot)
  rmdir(preferencesRoot, 's');
end
%=============================================================================
