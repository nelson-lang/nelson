%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonNan'), -1);
asserts.isequal(nargout('mustBeNonNan'), 0);
%=============================================================================
mustBeNonNan(1);
asserts.checkerror('mustBeNonNan(NaN)', sprintf(_('%s must not be NaN.'), 'Value'), 'Nelson:validators:mustBeNonNan');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must not be NaN.'), 'Value')];
asserts.checkerror('mustBeNonNan(NaN, 3)', msg, 'Nelson:validators:mustBeNonNan');
%=============================================================================
