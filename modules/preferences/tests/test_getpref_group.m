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
preferencesRoot = [tempdir(), 'preferences_group_', createGUID()];
mkdir(preferencesRoot, 's');
setenv('NELSON_PREFERENCES_PATH', preferencesRoot);
%=============================================================================
% Unique group, always removed even when an assertion fails.
group = ['nelson_getpref_group_', strrep(createGUID(), '-', '_')];
try
  % Missing group gives an empty double.
  R = getpref(group);
  asserts.isequal(class(R), 'double');
  asserts.isequal(size(R), [0 0]);
  asserts.isequal(getpref('9invalid_group_name'), []);
  %=============================================================================
  addpref(group, 'beta', 2);
  addpref(group, 'alpha', 'x');
  R = getpref(group);
  asserts.istrue(isstruct(R));
  asserts.isequal(size(R), [1 1]);
  asserts.isequal(fieldnames(R), {'beta'; 'alpha'});
  asserts.isequal(R.beta, 2);
  asserts.isequal(R.alpha, 'x');
  asserts.isequal(getpref(string(group)), R);
  %=============================================================================
  setpref(group, 'beta', uint8([1 2]));
  R = getpref(group);
  asserts.isequal(R.beta, uint8([1 2]));
  %=============================================================================
  rmpref(group, 'alpha');
  asserts.isequal(fieldnames(getpref(group)), {'beta'});
  rmpref(group);
  asserts.isequal(getpref(group), []);
  %=============================================================================
  msg = 'Group name must be a character vector or a scalar string.';
  asserts.checkerror('getpref(1)', msg, 'Nelson:preferences:invalidGroupType');
  asserts.checkerror('getpref({''a''})', msg, 'Nelson:preferences:invalidGroupType');
  asserts.checkerror('getpref(["a", "b"])', msg, 'Nelson:preferences:invalidGroupType');
catch e
  if ispref(group)
    rmpref(group);
  end
  setenv('NELSON_PREFERENCES_PATH', previousPreferencesPath);
  rethrow(e);
end
setenv('NELSON_PREFERENCES_PATH', previousPreferencesPath);
%=============================================================================
