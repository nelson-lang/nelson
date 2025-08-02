%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBePositive'), -1);
assert_isequal(nargout('mustBePositive'), 0);
%=============================================================================
mustBePositive([1, 2]);
assert_checkerror('mustBePositive(-Inf)', _('Value must be positive.'), 'Nelson:validators:mustBePositive');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be positive.')];
assert_checkerror('mustBePositive(-Inf, 3)', msg, 'Nelson:validators:mustBePositive');
%=============================================================================
