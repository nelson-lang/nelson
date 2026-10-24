%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = -intmax('int32') - intmax('int32');
REF = intmin('int32');
asserts.isequal(R, REF);
%=============================================================================
A = 1:2;
B = (1:3)';
R = int32(A) - int32(B);
REF = int32([0 1; -1 0; -2 -1]);
asserts.isequal(R, REF);
%=============================================================================
R = char(65) - int8([1 2]);
REF = int8([64 63]);
asserts.isequal(class(R), 'int8');
asserts.isequal(R, REF);
%=============================================================================
R = int8([1 2]) - char(65);
REF = int8([-64 -63]);
asserts.isequal(class(R), 'int8');
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('R = char([65 66]) - int8(1);', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
%=============================================================================
