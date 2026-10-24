%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonnegative'), -1);
asserts.isequal(nargout('mustBeNonnegative'), 0);
%=============================================================================
mustBePositive([1, 2]);
asserts.checkerror('mustBeNonnegative(-Inf)', sprintf(_('%s must be nonnegative.'), 'Value'), 'Nelson:validators:mustBeNonnegative');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be nonnegative.'), 'Value')];
asserts.checkerror('mustBeNonnegative(-Inf, 3)', msg, 'Nelson:validators:mustBeNonnegative');
%=============================================================================
