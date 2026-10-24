%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonpositive'), -1);
asserts.isequal(nargout('mustBeNonpositive'), 0);
%=============================================================================
mustBeNonpositive([-1, -2]);
asserts.checkerror('mustBeNonpositive(Inf)', sprintf(_('%s must be nonpositive.'), 'Value'), 'Nelson:validators:mustBeNonpositive');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be nonpositive.'), 'Value')];
asserts.checkerror('mustBeNonpositive(Inf, 3)', msg, 'Nelson:validators:mustBeNonpositive');
%=============================================================================
