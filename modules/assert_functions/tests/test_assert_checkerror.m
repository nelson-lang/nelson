%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_checkerror('cos', _('Wrong number of input arguments.'));
[r, msg] = assert_checkerror('cos', _('Wrong number of input arguments.'));
if (r ~= true)
  error(_('assert_checkerror fails.'));
end
if strcmp(msg, '') == false
  error(_('assert_checkerror fails.'));
end
%=============================================================================
[r, msg] = assert_checkerror('cos', 'Wrong number of input arguments2.');
if (r ~= false)
  error(_('assert_checkerror fails.'));
end
%=============================================================================
expectedmsg = [_('Assertion failed : expected error message ='), ' "', 'Wrong number of input arguments2.', '" ', _('computed error message ='), ' "', _('Wrong number of input arguments.'), '"'];
%=============================================================================
if strcmp(msg, expectedmsg) == false
  error(_('assert_checkerror fails.'));
end
%=============================================================================
assert_checkerror('mustBeFinite(NaN)', _('Value must be finite.'), 'Nelson:validators:mustBeFinite')
