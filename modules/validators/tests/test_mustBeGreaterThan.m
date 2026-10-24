%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeGreaterThan'), -2);
asserts.isequal(nargout('mustBeGreaterThan'), 0);
%=============================================================================
mustBeGreaterThan(2, 1);
%=============================================================================
msg = sprintf(_('Value must exceed %s.'), '1');
asserts.checkerror('mustBeGreaterThan(1, 1)', msg, 'Nelson:validators:mustBeGreaterThan');
%=============================================================================
mustBeGreaterThan([1 2 3], 0);
mustBeGreaterThan([], 5);
mustBeGreaterThan(zeros(0, 3), [1 2]);
%=============================================================================
% comparison array with a compatible size
mustBeGreaterThan([1 2 3], [0 1 2]);
mustBeGreaterThan([1 2 3; 4 5 6], [0; 3]);
msg = _('Value must exceed the compared value.');
asserts.checkerror('mustBeGreaterThan([1 2 3], [0 5 0])', msg, 'Nelson:validators:mustBeGreaterThan');
asserts.checkerror('mustBeGreaterThan([1 2 3; 4 5 6], [0; 4])', msg, 'Nelson:validators:mustBeGreaterThan');
msg = [sprintf(_('Invalid input argument at position %d.'), 2), char(10), _('Value must exceed the compared value.')];
asserts.checkerror('mustBeGreaterThan([1 2], [0 5], 2)', msg, 'Nelson:validators:mustBeGreaterThan');
hasFailed = false;
try
  mustBeGreaterThan([1 2 3], [0 1]);
catch
  hasFailed = true;
end
asserts.isequal(hasFailed, true);
%=============================================================================
% any type supported by gt
mustBeGreaterThan('b', 'a');
mustBeGreaterThan(["b", "c"], "a");
mustBeGreaterThan(uint8([3 4]), uint8(2));
asserts.checkerror('mustBeGreaterThan(int8(1), int8(4))', sprintf(_('Value must exceed %s.'), '4'), 'Nelson:validators:mustBeGreaterThan');
asserts.checkerror('mustBeGreaterThan(1, NaN)', sprintf(_('Value must exceed %s.'), 'NaN'), 'Nelson:validators:mustBeGreaterThan');
%=============================================================================
