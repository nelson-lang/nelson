%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeFinite'), -1);
asserts.isequal(nargout('mustBeFinite'), 0);
%=============================================================================
mustBeFinite(1);
asserts.checkerror('mustBeFinite(Inf)', sprintf(_('%s must be finite.'), 'Value'), 'Nelson:validators:mustBeFinite');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be finite.'), 'Value')];
asserts.checkerror('mustBeFinite(Inf, 3)', msg, 'Nelson:validators:mustBeFinite');
%=============================================================================
