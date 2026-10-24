%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeGreaterThanOrEqual'), -2);
asserts.isequal(nargout('mustBeGreaterThanOrEqual'), 0);
%=============================================================================
mustBeGreaterThanOrEqual(3, 3);
%=============================================================================
msg = sprintf(_('Value must be %s or greater.'), num2str(4));
asserts.checkerror('mustBeGreaterThanOrEqual(3, 4)', msg, 'Nelson:validators:mustBeGreaterThanOrEqual');
%=============================================================================
mustBeGreaterThanOrEqual([1 2 3], 1);
mustBeGreaterThanOrEqual([], 5);
%=============================================================================
% comparison array with a compatible size
mustBeGreaterThanOrEqual([1 2 3], [1 2 3]);
mustBeGreaterThanOrEqual([1 2 3; 2 3 4], [1; 2]);
msg = _('Value must be equal to or greater than the compared value.');
asserts.checkerror('mustBeGreaterThanOrEqual([1 2 3; 1 3 4], [1; 2])', msg, 'Nelson:validators:mustBeGreaterThanOrEqual');
asserts.checkerror('mustBeGreaterThanOrEqual([1 2 3], [1 2 4])', msg, 'Nelson:validators:mustBeGreaterThanOrEqual');
%=============================================================================
mustBeGreaterThanOrEqual('b', 'b');
asserts.checkerror('mustBeGreaterThanOrEqual(false, true)', sprintf(_('Value must be %s or greater.'), 'true'), 'Nelson:validators:mustBeGreaterThanOrEqual');
%=============================================================================
