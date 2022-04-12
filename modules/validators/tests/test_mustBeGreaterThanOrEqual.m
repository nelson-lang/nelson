%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeGreaterThanOrEqual'), -2);
assert_isequal(nargout('mustBeGreaterThanOrEqual'), 0);
%=============================================================================
mustBeGreaterThanOrEqual(3, 3);
%=============================================================================
msg = sprintf(_('Value must be greater than or equal to %s.'), num2str(4));
assert_checkerror('mustBeGreaterThanOrEqual(3, 4)', msg, 'Nelson:validators:mustBeGreaterThanOrEqual');
%=============================================================================
