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
% fft/ifft: integer/logical promotion, char rejection, scalar/empty with n,
% zero-padding, ifft 'symmetric' option
%=============================================================================
% integer and logical inputs are promoted to double
asserts.isequal(fft(int8([1 2])), [3 -1]);
asserts.isequal(class(fft(int8([1 2]))), 'double');
asserts.isequal(fft(uint16([1 2 3 4])), fft([1 2 3 4]));
asserts.isequal(fft(true), 1);
asserts.isequal(class(fft(true)), 'double');
asserts.isequal(ifft(int32([3 -1])), [1 2]);
%=============================================================================
% char input is rejected (aligned with expected behavior)
asserts.checkerror('fft(''ab'')', _('Invalid data type. First argument must be double, single, int8, uint8, int16, uint16, int32, uint32, or logical.'));
asserts.checkerror('ifft(''ab'')', _('Invalid data type. First argument must be double, single, int8, uint8, int16, uint16, int32, uint32, or logical.'));
%=============================================================================
% scalar with n is padded (n was previously ignored)
asserts.isequal(fft(5, 4), [5 5 5 5]);
asserts.isequal(ifft(4, 4), [1 1 1 1]);
asserts.isequal(fft(5, 4, 1), [5; 5; 5; 5]);
%=============================================================================
% n == 0 returns an empty result
asserts.isequal(size(fft([1 2 3], 0)), [1 0]);
asserts.isequal(class(fft([1 2 3], 0)), 'double');
asserts.isequal(size(fft([], 4)), [4 0]);
%=============================================================================
% zero-padding must pad with zeros (buffer was uninitialized)
asserts.isapprox(fft([1 2], 4), fft([1 2 0 0]), 1e-12);
asserts.isapprox(fft([1 2; 3 4], 4, 1), fft([1 2; 3 4; 0 0; 0 0], 4, 1), 1e-12);
%=============================================================================
% ifft 'symmetric' (treats input as conjugate symmetric, result is real)
asserts.isapprox(ifft([4 1 2 1], 'symmetric'), [2 0.5 1 0.5], 1e-12);
asserts.isapprox(ifft([4 1 2 1], 8, 'symmetric'), [1.5 0.5 0 0.5 0.5 0.5 0 0.5], 1e-12);
% the upper half of the spectrum is ignored
asserts.isapprox(ifft([4 1 2 9], 4, 'symmetric'), [2 0.5 1 0.5], 1e-12);
% imaginary part of the DC bin is ignored
asserts.isapprox(ifft([4 + 2i 1 2 9], 'symmetric'), [2 0.5 1 0.5], 1e-12);
asserts.istrue(isreal(ifft([4 + 2i 1 2 9], 'symmetric')));
% 'nonsymmetric' is the default behavior
asserts.isapprox(ifft([4 1 2 1], 'nonsymmetric'), ifft([4 1 2 1]), 1e-12);
% single precision stays single
asserts.isequal(class(ifft(single([4 1 2 1]), 'symmetric')), 'single');
% invalid flag
asserts.checkerror('ifft([1 2], ''badflag'')', _('Symmetry flag must be ''symmetric'' or ''nonsymmetric''.'));
% the symmetry-flag condition also carries a stable identifier
asserts.checkerror('ifft([1 2], ''badflag'')', _('Symmetry flag must be ''symmetric'' or ''nonsymmetric''.'), 'Nelson:ifft:invalidSymmetryFlag');
%=============================================================================
