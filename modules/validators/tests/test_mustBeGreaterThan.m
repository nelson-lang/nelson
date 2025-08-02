%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeGreaterThan'), -2);
assert_isequal(nargout('mustBeGreaterThan'), 0);
%=============================================================================
mustBeGreaterThan(2, 1);
%=============================================================================
msg = sprintf(_('Value must be greater than %d.'), 1);
assert_checkerror('mustBeGreaterThan(1, 1)', msg, 'Nelson:validators:mustBeGreaterThan');
%=============================================================================
