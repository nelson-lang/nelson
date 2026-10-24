%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% assignment-suppression flag '*' in sscanf conversion specifiers.
%=============================================================================
[A, n, errmsg] = sscanf('1 2 3', '%*d %d');
asserts.isequal(A, 2);
asserts.isequal(n, 1);
asserts.isequal(errmsg, '');
%=============================================================================
[A, n, errmsg] = sscanf('a 5', '%*s %d');
asserts.isequal(A, 5);
asserts.isequal(n, 1);
asserts.isequal(errmsg, '');
%=============================================================================
A = sscanf('abc123', '%*c%c');
asserts.isequal(A, 'b13');
%=============================================================================
A = sscanf('x1y2z3', '%*c%d');
asserts.isequal(A, [1; 2; 3]);
%=============================================================================
A = sscanf('1.5 2.5 3.5', '%*f %f');
asserts.isapprox(A, 2.5, 1e-4);
%=============================================================================
% width combined with suppression.
A = sscanf('11 22 33', '%*2d%d');
asserts.isequal(A, 22);
%=============================================================================
% length modifier combined with suppression.
A = sscanf('78 79 80', '%*ld %d');
asserts.isequal(A, 79);
%=============================================================================
% suppression in second field, repeated cycling of the format.
A = sscanf('10 20 30 40', '%d %*d');
asserts.isequal(A, [10; 30]);
%=============================================================================
A = sscanf('1 2 3 4 5 6', '%d %*d');
asserts.isequal(A, [1; 3; 5]);
%=============================================================================
% mixed suppressed and stored fields.
A = sscanf('foo 1 bar 2', '%*s %d');
asserts.isequal(A, [1; 2]);
%=============================================================================
% %*s skips a whole token before a stored string.
A = sscanf('skip keep', '%*s %s');
asserts.isequal(A, 'keep');
%=============================================================================
% nextindex accounts for the suppressed field.
[A, n, errmsg, nextindex] = sscanf('1 2 3', '%*d %d');
asserts.isequal(nextindex, 6);
%=============================================================================
% ordinary sscanf is unaffected.
A = sscanf('1 2 3', '%d');
asserts.isequal(A, [1; 2; 3]);
%=============================================================================
