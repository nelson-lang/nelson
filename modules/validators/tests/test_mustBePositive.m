%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBePositive'), -1);
asserts.isequal(nargout('mustBePositive'), 0);
%=============================================================================
mustBePositive([1, 2]);
asserts.checkerror('mustBePositive(-Inf)', sprintf(_('%s must be positive.'), 'Value'), 'Nelson:validators:mustBePositive');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be positive.'), 'Value')];
asserts.checkerror('mustBePositive(-Inf, 3)', msg, 'Nelson:validators:mustBePositive');
%=============================================================================
