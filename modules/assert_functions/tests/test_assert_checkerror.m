%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_checkerror('cos', _('Not enough input arguments.'));
[r, msg] = assert_checkerror('cos', _('Not enough input arguments.'));
if (r ~= true)
  error('Nelson:asserts:testFailure', _('assert_checkerror fails.'));
end
if strcmp(msg, '') == false
  error('Nelson:asserts:testFailure', _('assert_checkerror fails.'));
end
%=============================================================================
[r, msg] = assert_checkerror('cos', 'Wrong number of input arguments2.');
if (r ~= false)
  error('Nelson:asserts:testFailure', _('assert_checkerror fails.'));
end
%=============================================================================
expectedmsg = sprintf(_('Assertion failed: expected error message = "%s" computed error message = "%s"'), 'Wrong number of input arguments2.', _('Not enough input arguments.'));
%=============================================================================
if strcmp(msg, expectedmsg) == false
  error('Nelson:asserts:testFailure', _('assert_checkerror fails.'));
end
%=============================================================================
assert_checkerror('mustBeFinite(NaN)', _('Value must be finite.'), 'Nelson:validators:mustBeFinite')
%=============================================================================
[r, msg] = assert_checkerror('1 + 1', _('unused'));
assert_isfalse(r);
assert_isequal(msg, _('No error was produced while evaluating command.'));
%=============================================================================
assert_checkerror('assert_checkerror(''1 + 1'', _(''unused''));', _('No error was produced while evaluating command.'));
