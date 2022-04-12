%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeLessThanOrEqual'), -2);
assert_isequal(nargout('mustBeLessThanOrEqual'), 0);
%=============================================================================
mustBeLessThanOrEqual(3, 3);
%=============================================================================
msg = sprintf(_('Value must be less than or equal to %s.'), num2str(2));
assert_checkerror('mustBeLessThanOrEqual(3, 2)', msg, 'Nelson:validators:mustBeLessThanOrEqual');
%=============================================================================
