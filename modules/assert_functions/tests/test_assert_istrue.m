%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_istrue(true);
r = assert_istrue(true);
if (r ~= true)
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
[r, msg] = assert_istrue(true);
if (r ~= true)
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
if strcmp(msg, '') == false
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
r = assert_istrue(false);
if (r ~= false)
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
[r, msg] = assert_istrue(false);
if (r ~= false)
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
expectedmsg = getString(message('nelson:assert:assertionFailedValueExpectedComputed', true, false));
if strcmp(msg, expectedmsg) == false
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
[r, msg] = assert_istrue(false, _('your error message'));
if (r ~= false)
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
if strcmp(msg, _('your error message')) == false
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
try
  assert_istrue(false);
catch
  err = lasterror();
  expectedmsg = getString(message('nelson:assert:assertionFailedValueExpectedComputed', true, false));
  if strcmp(err.message, expectedmsg) == false
    error(message('nelson:assert:assertionFailed'));
  end
end
%=============================================================================
