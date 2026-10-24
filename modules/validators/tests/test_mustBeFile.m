%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeFile'), -1);
asserts.isequal(nargout('mustBeFile'), 0);
%=============================================================================
mustBeFile([nelsonroot(), '/gpl-3.0.md']);
asserts.checkerror('mustBeFile(Inf)', sprintf(_('%s must be a character vector or string scalar.'), 'Value'), 'Nelson:validators:mustBeTextScalar');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a file.'), 'Value')];
asserts.checkerror('mustBeFile(''Inf'', 3)', msg, 'Nelson:validators:mustBeFile');
%=============================================================================
