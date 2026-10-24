%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeLessThan'), -2);
asserts.isequal(nargout('mustBeLessThan'), 0);
%=============================================================================
mustBeLessThan(2, 3);
%=============================================================================
msg = sprintf(_('Value must be below %s.'), num2str(0));
asserts.checkerror('mustBeLessThan(3, 0)', msg, 'Nelson:validators:mustBeLessThan');
%=============================================================================
mustBeLessThan([1 2 3], 4);
mustBeLessThan([], -5);
%=============================================================================
% comparison array with a compatible size
mustBeLessThan([4 9 14], [5 10 15]);
mustBeLessThan([1; 2], [3 4]);
msg = _('Value must be below the compared value.');
asserts.checkerror('mustBeLessThan([4 9 15], [5 10 15])', msg, 'Nelson:validators:mustBeLessThan');
asserts.checkerror('mustBeLessThan([1; 3], [2 4])', msg, 'Nelson:validators:mustBeLessThan');
%=============================================================================
mustBeLessThan("a", ["b", "c"]);
asserts.checkerror('mustBeLessThan(true, false)', sprintf(_('Value must be below %s.'), 'false'), 'Nelson:validators:mustBeLessThan');
%=============================================================================
