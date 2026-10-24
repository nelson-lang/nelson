%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNumericOrLogical'), -1);
asserts.isequal(nargout('mustBeNumericOrLogical'), 0);
%=============================================================================
mustBeNumericOrLogical(1);
asserts.checkerror('mustBeNumericOrLogical(struct())', sprintf(_('%s must be numeric or logical.'), 'Value'), 'Nelson:validators:mustBeNumericOrLogical');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be numeric or logical.'), 'Value')];
asserts.checkerror('mustBeNumericOrLogical(struct(), 3)', msg, 'Nelson:validators:mustBeNumericOrLogical');
%=============================================================================
