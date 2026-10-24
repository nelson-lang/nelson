%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('assert_isfalse'), -1);
assert_isequal(nargout('assert_isfalse'), 2);
%=============================================================================
assert_isfalse(false);
%=============================================================================
r = assert_isfalse(false);
if (r ~= true)
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
[r, msg] = assert_isfalse(false);
if (r ~= true)
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
if strcmp(msg, '') == false
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
r = assert_isfalse(true);
if (r ~= false)
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
[r, msg] = assert_isfalse(true);
if (r ~= false)
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
if strcmp(msg, _('Assertion failed: found true entry in condition = true.')) == false
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
[r, msg] = assert_isfalse(true, _('your error message'));
if (r ~= false)
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
if strcmp(msg, _('your error message')) == false
  error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
end
%=============================================================================
try
  assert_isfalse(true);
catch
  err = lasterror();
  if strcmp(err.message, _('Assertion failed: found true entry in condition = true.')) == false
    error('Nelson:asserts:testFailure', _('assert_isfalse fails.'));
  end
end
%=============================================================================
