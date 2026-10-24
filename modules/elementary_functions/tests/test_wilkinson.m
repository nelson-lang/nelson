%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('wilkinson'), -1)
asserts.isequal(nargout('wilkinson'), 1)
%=============================================================================
R = wilkinson([]);
REF = [];
asserts.isequal(R, REF);
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 1), char(10), _('Value must be an integer.')];
asserts.checkerror('R = wilkinson(NaN)', msg);
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 1), char(10), _('Value must be an integer.')];
asserts.checkerror('R = wilkinson(NaN)', msg);
%=============================================================================
R = wilkinson(0);
REF = [];
asserts.isequal(R, REF);
%=============================================================================
R = wilkinson(1);
REF = 0;
asserts.isequal(R, REF);
%=============================================================================
R = wilkinson(2);
REF = [0.5, 1; 1, 0.5];
asserts.isequal(R, REF);
%=============================================================================
R = wilkinson(3);
REF = [1, 1, 0; 1, 0, 1; 0, 1, 1];
asserts.isequal(R, REF);
%=============================================================================
R = wilkinson(4);
REF = [1.5, 1, 0, 0; 1, 0.5, 1, 0; 0, 1, 0.5, 1; 0, 0, 1, 1.5];
asserts.isequal(R, REF);
%=============================================================================
