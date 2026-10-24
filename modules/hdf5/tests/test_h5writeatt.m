%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('h5writeatt'), -1);
asserts.isequal(nargout('h5writeatt'), 0);
%=============================================================================
h5filename = [tempname(), '.h5'];
if isfile(h5filename) rmfile(h5filename) end
h5create(h5filename, '/value', [100 200]);
%=============================================================================
REF = '20-Dec-2018 19:46:41';
h5writeatt(h5filename, '/', 'creation_date', REF);
R = h5readatt(h5filename, '/', 'creation_date');
asserts.isequal(R, REF);
%=============================================================================
REF = '';
h5writeatt(h5filename, '/', 'empty_str', REF);
R = h5readatt(h5filename, '/', 'empty_str');
asserts.isequal(R, REF);
%=============================================================================
REF = eye(3, 4);
h5writeatt(h5filename, '/', 'eye_double_1', REF);
R = h5readatt(h5filename, '/', 'eye_double_1');
asserts.isequal(R, REF);
%=============================================================================
S = zeros(3, 0);
h5writeatt(h5filename, '/', 'empty_double_1', S);
R = h5readatt(h5filename, '/', 'empty_double_1');
REF = [];
asserts.isequal(R, REF);
%=============================================================================
S = [];
h5writeatt(h5filename, '/', 'empty_double_2', S);
R = h5readatt(h5filename, '/', 'empty_double_2');
REF = [];
asserts.isequal(R, REF);
%=============================================================================
REF = complex([], []);
h5writeatt(h5filename, '/', 'dcomplex_empty', REF);
R = h5readatt(h5filename, '/', 'dcomplex_empty');
asserts.isequal(R, []);
%=============================================================================
REF = 0;
h5writeatt(h5filename, '/', 'double_zero', REF);
R = h5readatt(h5filename, '/', 'double_zero');
asserts.isequal(R, REF);
%=============================================================================
REF = 3;
h5writeatt(h5filename, '/', 'double_three', REF);
R = h5readatt(h5filename, '/', 'double_three');
asserts.isequal(R, REF);
%=============================================================================
REF = rand(3, 2, 4);
h5writeatt(h5filename, '/', 'double_3d', REF);
R = h5readatt(h5filename, '/', 'double_3d');
asserts.isapprox(R, REF, 1e-8);
%=============================================================================
REF = single(eye(3, 4));
h5writeatt(h5filename, '/', 'eye_single_1', REF);
R = h5readatt(h5filename, '/', 'eye_single_1');
asserts.isequal(R, REF);
%=============================================================================
S = single(zeros(3, 0));
h5writeatt(h5filename, '/', 'empty_single_1', S);
R = h5readatt(h5filename, '/', 'empty_single_1');
REF = single([]);
asserts.isequal(R, REF);
%=============================================================================
S = single([]);
h5writeatt(h5filename, '/', 'empty_single_2', S);
R = h5readatt(h5filename, '/', 'empty_single_2');
REF = single([]);
asserts.isequal(R, REF);
%=============================================================================
REF = single(complex([], []));
h5writeatt(h5filename, '/', 'scomplex_empty', REF);
R = h5readatt(h5filename, '/', 'scomplex_empty');
asserts.isequal(R, single([]));
%=============================================================================
REF = single(0);
h5writeatt(h5filename, '/', 'single_zero', REF);
R = h5readatt(h5filename, '/', 'single_zero');
asserts.isequal(R, REF);
%=============================================================================
REF = single(3);
h5writeatt(h5filename, '/', 'single_three', REF);
R = h5readatt(h5filename, '/', 'single_three');
asserts.isequal(R, REF);
%=============================================================================
REF = single(rand(3, 2, 4));
h5writeatt(h5filename, '/', 'single_3d', REF);
R = h5readatt(h5filename, '/', 'single_3d');
asserts.isapprox(R, REF, 1e-8);
%=============================================================================
REF = int8(33);
h5writeatt(h5filename, '/', 'int8_1', REF);
R = h5readatt(h5filename, '/', 'int8_1');
asserts.isequal(R, REF);
%=============================================================================
REF = int16(33);
h5writeatt(h5filename, '/', 'int16_1', REF);
R = h5readatt(h5filename, '/', 'int16_1');
asserts.isequal(R, REF);
%=============================================================================
REF = int32(33);
h5writeatt(h5filename, '/', 'int32_1', REF);
R = h5readatt(h5filename, '/', 'int32_1');
asserts.isequal(R, REF);
%=============================================================================
REF = int64(33);
h5writeatt(h5filename, '/', 'int64_1', REF);
R = h5readatt(h5filename, '/', 'int64_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint8(33);
h5writeatt(h5filename, '/', 'uint8_1', REF);
R = h5readatt(h5filename, '/', 'uint8_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint16(33);
h5writeatt(h5filename, '/', 'uint16_1', REF);
R = h5readatt(h5filename, '/', 'uint16_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint32(33);
h5writeatt(h5filename, '/', 'uint32_1', REF);
R = h5readatt(h5filename, '/', 'uint32_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint64(33);
h5writeatt(h5filename, '/', 'uint64_1', REF);
R = h5readatt(h5filename, '/', 'uint64_1');
asserts.isequal(R, REF);
%=============================================================================
if isfile(h5filename) rmfile(h5filename) end
%=============================================================================
