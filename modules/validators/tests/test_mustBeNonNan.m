%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonNan'), -1);
assert_isequal(nargout('mustBeNonNan'), 0);
%=============================================================================
mustBeNonNan(1);
assert_checkerror('mustBeNonNan(NaN)', _('Value must not be NaN.'), 'Nelson:validators:mustBeNonNan');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must not be NaN.')];
assert_checkerror('mustBeNonNan(NaN, 3)', msg, 'Nelson:validators:mustBeNonNan');
%=============================================================================
