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
% NELSON_PREFERENCES_PATH set by setenv must redirect the store file.
previousPreferencesPath = getenv('NELSON_PREFERENCES_PATH');
preferencesRoot = [tempdir(), 'preferences_override_', createGUID()];
mkdir(preferencesRoot, 's');
cleanupPreferencesPath = onCleanup(@() setenv('NELSON_PREFERENCES_PATH', previousPreferencesPath)); %#ok<NLS0004>
setenv('NELSON_PREFERENCES_PATH', preferencesRoot);
%=============================================================================
storeFile = fullfile(preferencesRoot, 'preferences.nh5');
asserts.isfalse(isfile(storeFile));
setpref('nelson_pref_override_test', 'value', 42);
asserts.istrue(isfile(storeFile));
asserts.isequal(getpref('nelson_pref_override_test', 'value'), 42);
rmpref('nelson_pref_override_test');
asserts.isfalse(ispref('nelson_pref_override_test'));
%=============================================================================
