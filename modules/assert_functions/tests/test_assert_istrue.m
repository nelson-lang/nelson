%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('assert_istrue'), -1);
assert_isequal(nargout('assert_istrue'), 2);
%=============================================================================
assert_istrue(true);
assert_checkerror('assert_istrue(false)', _('Assertion failed: found false entry in condition = false.'), 'Nelson:asserts:assertionFailed');
r = assert_istrue(true);
if (r ~= true)
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
[r, msg] = assert_istrue(true);
if (r ~= true)
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
if strcmp(msg, '') == false
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
r = assert_istrue(false);
if (r ~= false)
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
[r, msg] = assert_istrue(false);
if (r ~= false)
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
if strcmp(msg, _('Assertion failed: found false entry in condition = false.')) == false
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
[r, msg] = assert_istrue(false, _('your error message'));
if (r ~= false)
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
if strcmp(msg, _('your error message')) == false
  error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
end
%=============================================================================
try
  assert_istrue(false);
catch
  err = lasterror();
  if strcmp(err.message, _('Assertion failed: found false entry in condition = false.')) == false
    error('Nelson:asserts:testFailure', _('assert_istrue fails.'));
  end
end
%=============================================================================
