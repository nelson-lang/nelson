%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('isprime(true)', _('Input must contain nonnegative integers.'));
asserts.isequal(isprime(uint64(18446744073709551615)), false);
asserts.isequal(isprime(intmax('int64')), false);
asserts.isequal(isprime(sparse([2 3 4])), logical([1 1 0]));
asserts.checkerror('isprime({1})', _('Check for incorrect argument data type or missing argument in call to function ''isprime''.'));
asserts.isequal(isprime(7.0), true);
asserts.isequal(isprime(-0), false);
asserts.isequal(isprime(1e10+19), true);
asserts.isequal(isprime(uint8(255)), false);
asserts.isequal(isprime(int8(127)), true);
asserts.isequal(isprime(97*89), false);
asserts.isequal(isprime(2147483647), true);
asserts.isequal(isprime(4294967291), true);
asserts.isequal(isprime(single(16777213)), true);
asserts.isequal(isprime(single(16777216)), false);
asserts.isequal(isprime(9007199254740881), true);
asserts.isequal(isprime(9007199254740992), false);
asserts.isequal(isprime(single(2^24+1)), false);
asserts.isequal(isprime(ones(2,3,2)), logical(reshape([0 0 0 0 0 0 0 0 0 0 0 0], [2 3 2])));
asserts.isequal(isprime(2), true);
asserts.isequal(isprime(3), true);
asserts.isequal(isprime(4), false);
asserts.checkerror('isprime(-2)', _('Input must contain nonnegative integers.'));
asserts.checkerror('isprime(false)', _('Input must contain nonnegative integers.'));
asserts.checkerror('isprime(''abc'')', _('Input must contain nonnegative integers.'));
asserts.checkerror('isprime("a")', _('Check for incorrect argument data type or missing argument in call to function ''isprime''.'));
asserts.isequal(isprime(uint8(0:20)), logical([0 0 1 1 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0]));
asserts.checkerror('isprime(int8(-5:20))', _('Input must contain nonnegative integers.'));
asserts.isequal(isprime(single(1:30)), logical([0 1 1 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0]));
asserts.isequal(isprime(1:30), logical([0 1 1 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0]));
asserts.checkerror('isprime(1.0000000000000002)', _('Input must contain nonnegative integers.'));
asserts.checkerror('isprime(2.0000000000000004)', _('Input must contain nonnegative integers.'));
asserts.checkerror('isprime(-Inf)', _('Input must contain nonnegative integers.'));
asserts.checkerror('isprime([1 NaN])', _('Input must contain nonnegative integers.'));
asserts.isequal(isprime(uint16(65521)), true);
asserts.isequal(isprime(uint16(65535)), false);
asserts.isequal(isprime(int16(32749)), true);
asserts.isequal(isprime(uint32(4294967291)), true);
asserts.isequal(isprime(uint32(4294967295)), false);
asserts.isequal(isprime(int32(2147483647)), true);
asserts.isequal(isprime(uint64(2)^64-59), false);
asserts.isequal(isprime(uint64(4294967291)*uint64(3)), false);
asserts.isequal(isprime(single(2)), true);
asserts.isequal(isprime(sparse(7)), true);
asserts.isequal(isprime(sparse(zeros(2,2))), logical(reshape([0 0 0 0], [2 2])));
asserts.isequal(isprime(complex(7,0)), true);
asserts.isequal(isprime(7+0i), true);
asserts.checkerror('isprime(1i)', _('Input must contain nonnegative integers.'));
asserts.checkerror('isprime(1:3,2)', _('Too many input arguments.'));
asserts.checkerror('isprime()', _('Not enough input arguments.'));
asserts.isequal(isprime(1000000007), true);
asserts.isequal(isprime(1000000009), true);
asserts.isequal(isprime(intmax('uint64')-58), true);
asserts.isequal(isprime(intmax('uint64')), false);
asserts.isequal(isprime(intmax('uint64')-1), false);
asserts.isequal(isprime(intmax('int64')-24), true);
asserts.isequal(isprime(uint64(4294967291)), true);
asserts.isequal(isprime(uint64(4294967311)), true);
asserts.isequal(isprime(uint64(4294967291)*uint64(4294967279)), false);
asserts.isequal(isprime(uint64(4294967291)*uint64(4294967291)), false);
asserts.isequal(isprime(uint64(3037000493)*uint64(3037000453)), false);
asserts.isequal(isprime(1e15+37), true);
asserts.isequal(isprime(1e15+1), false);
asserts.isequal(isprime(999999999989), true);
asserts.isequal(isprime(2^53-1), false);
asserts.isequal(isprime(uint64([2 3 4 5 6 7 8 9 10 11])), logical([1 1 0 1 0 1 0 0 0 1]));
asserts.checkerror('isprime(int64([-1 0 1 2 3]))', _('Input must contain nonnegative integers.'));
asserts.isequal(isprime(sparse([2 3 4 5])), logical([1 1 0 1]));
%=============================================================================
