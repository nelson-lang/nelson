%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_checkerror('cos', message('nelson:arguments:tooFewInputs'));
[r, msg] = assert_checkerror('cos', message('nelson:arguments:tooFewInputs'));
if (r ~= true)
  error(message('nelson:assert:assertionFailed'));
end
if strcmp(msg, '') == false
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
[r, msg] = assert_checkerror('cos', 'Wrong number of input arguments2.');
if (r ~= false)
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
expectedmsg = getString(message('nelson:assert:assertionFailedMessageExpectedComputed', _('Wrong number of input arguments2.'), getString(message('nelson:arguments:tooFewInputs'))));
%=============================================================================
if strcmp(msg, expectedmsg) == false
  error(message('nelson:assert:assertionFailed'));
end
%=============================================================================
assert_checkerror('mustBeFinite(NaN)', _('Value must be finite.'), 'nelson:validators:mustBeFinite')
%=============================================================================
assert_checkerror('mustBeFinite(NaN)', message('nelson:validators:mustBeFinite'));
%=============================================================================
