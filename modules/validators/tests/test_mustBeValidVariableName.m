%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeValidVariableName'), -1);
asserts.isequal(nargout('mustBeValidVariableName'), 0);
%=============================================================================
mustBeValidVariableName('t8');
mustBeValidVariableName("t8");
%=============================================================================
asserts.checkerror('mustBeValidVariableName(1)', sprintf(_('%s must be a valid variable name.'), 'Value'));
%=============================================================================
asserts.checkerror('mustBeValidVariableName("8t")', sprintf(_('%s must be a valid variable name.'), 'Value'));
%=============================================================================
asserts.checkerror('mustBeValidVariableName("8t")', sprintf(_('%s must be a valid variable name.'), 'Value'), 'Nelson:validators:mustBeValidVariableName');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a valid variable name.'), 'Value')];
asserts.checkerror('mustBeValidVariableName("8t", 3)', msg);
%=============================================================================
