%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeLessThanOrEqual'), -2);
asserts.isequal(nargout('mustBeLessThanOrEqual'), 0);
%=============================================================================
mustBeLessThanOrEqual(3, 3);
%=============================================================================
msg = sprintf(_('Value must be %s or less.'), num2str(2));
asserts.checkerror('mustBeLessThanOrEqual(3, 2)', msg, 'Nelson:validators:mustBeLessThanOrEqual');
%=============================================================================
mustBeLessThanOrEqual([1 2 3], 3);
mustBeLessThanOrEqual([], -5);
%=============================================================================
% comparison array with a compatible size
mustBeLessThanOrEqual([0 1; 2 2], [1; 2]);
msg = _('Value must be equal to or less than the compared value.');
asserts.checkerror('mustBeLessThanOrEqual([0 1; 2 3], [1; 2])', msg, 'Nelson:validators:mustBeLessThanOrEqual');
asserts.checkerror('mustBeLessThanOrEqual(single([1 2]), single([1 1]))', msg, 'Nelson:validators:mustBeLessThanOrEqual');
%=============================================================================
mustBeLessThanOrEqual('a', 'a');
%=============================================================================
