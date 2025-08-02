%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeLessThan'), -2);
assert_isequal(nargout('mustBeLessThan'), 0);
%=============================================================================
mustBeLessThan(2, 3);
%=============================================================================
msg = sprintf(_('Value must be less than %s.'), num2str(0));
assert_checkerror('mustBeLessThan(3, 0)', msg, 'Nelson:validators:mustBeLessThan');
%=============================================================================
