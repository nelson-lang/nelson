%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isMATLABReleaseOlderThan'), -2);
assert_isequal(nargout('isMATLABReleaseOlderThan'), 1);
%=============================================================================
% Return type is always a logical scalar.
r = isMATLABReleaseOlderThan('R2023b');
assert_istrue(islogical(r));
assert_istrue(isscalar(r));
%=============================================================================
% The emulated release is R2025a: older releases resolve to false.
assert_isequal(isMATLABReleaseOlderThan('R2009b'), false);
assert_isequal(isMATLABReleaseOlderThan('R2023a'), false);
assert_isequal(isMATLABReleaseOlderThan('R2023b'), false);
assert_isequal(isMATLABReleaseOlderThan('R2024b'), false);
assert_isequal(isMATLABReleaseOlderThan('R2025a'), false); % equal is not older
%=============================================================================
% Truly newer releases resolve to true.
assert_isequal(isMATLABReleaseOlderThan('R2025b'), true);
assert_isequal(isMATLABReleaseOlderThan('R2026a'), true);
%=============================================================================
% Nelson is treated as carrying every update of the emulated release.
assert_isequal(isMATLABReleaseOlderThan('R2025a', 'update', 5), false);
assert_isequal(isMATLABReleaseOlderThan('R2030a', 'update', 5), true);
%=============================================================================
% String scalar inputs are accepted.
assert_isequal(isMATLABReleaseOlderThan("R2025b"), true);
%=============================================================================
% Malformed release strings error.
assert_checkerror('isMATLABReleaseOlderThan(''2023a'')', _('The release must be of the form ''RYYYYa'' or ''RYYYYb'', such as ''R2023b''.'));
assert_checkerror('isMATLABReleaseOlderThan(''R2023c'')', _('The release must be of the form ''RYYYYa'' or ''RYYYYb'', such as ''R2023b''.'));
%=============================================================================
% Bad option / update value error.
assert_checkerror('isMATLABReleaseOlderThan(''R2025a'', ''foo'', 1)', _('The option must be ''update'' followed by an integer value.'));
assert_checkerror('isMATLABReleaseOlderThan(''R2025a'', ''update'', -1)', _('The update value must be a nonnegative integer.'));
%=============================================================================
% Wrong argument type errors.
assert_checkerror('isMATLABReleaseOlderThan(1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
%=============================================================================
