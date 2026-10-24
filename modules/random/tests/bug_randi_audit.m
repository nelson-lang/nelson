%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% randi: limits validation and 64-bit ranges (was truncated by an int cast)
%=============================================================================
msg = _('First input must be a positive scalar integer value IMAX, or two integer values [IMIN IMAX] with IMIN less than or equal to IMAX.');
asserts.checkerror('randi(0)', msg);
asserts.checkerror('randi(-3)', msg);
asserts.checkerror('randi(2.5)', msg);
asserts.checkerror('randi([5 3])', msg);
asserts.checkerror('randi([1.5 3])', msg);
asserts.checkerror('randi(NaN)', msg);
asserts.checkerror('randi(Inf)', msg);
%=============================================================================
asserts.checkerror('randi(2^53)', _('IMIN and IMAX must be less than 2^53.'));
asserts.checkerror('randi([-2^53, 10])', _('IMIN and IMAX must be less than 2^53.'));
% the range condition also carries a stable identifier
asserts.checkerror('randi(2^53)', _('IMIN and IMAX must be less than 2^53.'), 'Nelson:randi:rangeTooLarge');
%=============================================================================
% large ranges are no longer truncated to 32 bits
v = randi(2 ^ 40, 1000, 1);
asserts.istrue(all(v >= 1));
asserts.istrue(all(v <= 2 ^ 40));
asserts.istrue(all(v == floor(v)));
% overwhelmingly likely with 1000 draws over [1, 2^40]
asserts.istrue(max(v) > 2 ^ 32);
%=============================================================================
v = randi(2 ^ 53 - 1, 100, 1);
asserts.istrue(all(v >= 1));
asserts.istrue(all(v <= 2 ^ 53 - 1));
%=============================================================================
% ranges with imin different from 1, including negative values
v = randi([5 10], 100, 1);
asserts.istrue(all(v >= 5));
asserts.istrue(all(v <= 10));
v = randi([-5 5], 100, 1);
asserts.istrue(all(v >= -5));
asserts.istrue(all(v <= 5));
asserts.isequal(class(v), 'double');
%=============================================================================
% degenerate but valid range
asserts.isequal(randi([7 7]), 7);
asserts.isequal(randi(1), 1);
%=============================================================================
% still works with type argument
v = randi(10, 2, 2, 'int8');
asserts.isequal(class(v), 'int8');
%=============================================================================
