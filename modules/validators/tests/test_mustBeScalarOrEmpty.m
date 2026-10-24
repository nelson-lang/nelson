%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeScalarOrEmpty'), -1);
asserts.isequal(nargout('mustBeScalarOrEmpty'), 0);
%=============================================================================
mustBeScalarOrEmpty(1);
mustBeScalarOrEmpty([]);
asserts.checkerror('mustBeScalarOrEmpty([1 2])', sprintf(_('%s must be a scalar or empty.'), 'Value'), 'Nelson:validators:mustBeScalarOrEmpty');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a scalar or empty.'), 'Value')];
asserts.checkerror('mustBeScalarOrEmpty([1 2], 3)', msg);
%=============================================================================
