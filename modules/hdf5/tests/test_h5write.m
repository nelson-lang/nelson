%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('h5write'), 3);
asserts.isequal(nargout('h5write'), 0);
%=============================================================================
h5filename = [tempname(), '.h5'];
if isfile(h5filename) rmfile(h5filename) end
%=============================================================================
REF = eye(3, 4);
h5write(h5filename, '/eye_double_1', REF);
R = h5read(h5filename, '/eye_double_1');
asserts.isequal(R, REF);
%=============================================================================
S = zeros(3, 0);
h5write(h5filename, '/empty_double_1', S);
R = h5read(h5filename, '/empty_double_1');
REF = [];
asserts.isequal(R, REF);
%=============================================================================
S = [];
h5write(h5filename, '/empty_double_2', S);
R = h5read(h5filename, '/empty_double_2');
REF = [];
asserts.isequal(R, REF);
%=============================================================================
REF = complex([], []);
h5write(h5filename, '/dcomplex_empty', REF);
R = h5read(h5filename, '/dcomplex_empty');
asserts.isequal(R, []);
%=============================================================================
REF = 0;
h5write(h5filename, '/double_zero', REF);
R = h5read(h5filename, '/double_zero');
asserts.isequal(R, REF);
%=============================================================================
REF = 3;
h5write(h5filename, '/double_three', REF);
R = h5read(h5filename, '/double_three');
asserts.isequal(R, REF);
%=============================================================================
REF = rand(3, 2, 4);
h5write(h5filename, '/double_3d', REF);
R = h5read(h5filename, '/double_3d');
asserts.isapprox(R, REF, 1e-8);
%=============================================================================
REF = single(eye(3, 4));
h5write(h5filename, '/eye_single_1', REF);
R = h5read(h5filename, '/eye_single_1');
asserts.isequal(R, REF);
%=============================================================================
S = single(zeros(3, 0));
h5write(h5filename, '/empty_single_1', S);
R = h5read(h5filename, '/empty_single_1');
REF = single([]);
asserts.isequal(R, REF);
%=============================================================================
S = single([]);
h5write(h5filename, '/empty_single_2', S);
R = h5read(h5filename, '/empty_single_2');
REF = single([]);
asserts.isequal(R, REF);
%=============================================================================
REF = single(complex([], []));
h5write(h5filename, '/scomplex_empty', REF);
R = h5read(h5filename, '/scomplex_empty');
asserts.isequal(R, single([]));
%=============================================================================
REF = single(0);
h5write(h5filename, '/single_zero', REF);
R = h5read(h5filename, '/single_zero');
asserts.isequal(R, REF);
%=============================================================================
REF = single(3);
h5write(h5filename, '/single_three', REF);
R = h5read(h5filename, '/single_three');
asserts.isequal(R, REF);
%=============================================================================
REF = single(rand(3, 2, 4));
h5write(h5filename, '/single_3d', REF);
R = h5read(h5filename, '/single_3d');
asserts.isapprox(R, REF, 1e-8);
%=============================================================================
REF = int8(33);
h5write(h5filename, '/int8_1', REF);
R = h5read(h5filename, '/int8_1');
asserts.isequal(R, REF);
%=============================================================================
REF = int16(33);
h5write(h5filename, '/int16_1', REF);
R = h5read(h5filename, '/int16_1');
asserts.isequal(R, REF);
%=============================================================================
REF = int32(33);
h5write(h5filename, '/int32_1', REF);
R = h5read(h5filename, '/int32_1');
asserts.isequal(R, REF);
%=============================================================================
REF = int64(33);
h5write(h5filename, '/int64_1', REF);
R = h5read(h5filename, '/int64_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint8(33);
h5write(h5filename, '/uint8_1', REF);
R = h5read(h5filename, '/uint8_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint16(33);
h5write(h5filename, '/uint16_1', REF);
R = h5read(h5filename, '/uint16_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint32(33);
h5write(h5filename, '/uint32_1', REF);
R = h5read(h5filename, '/uint32_1');
asserts.isequal(R, REF);
%=============================================================================
REF = uint64(33);
h5write(h5filename, '/uint64_1', REF);
R = h5read(h5filename, '/uint64_1');
asserts.isequal(R, REF);
%=============================================================================
REF = '20-Dec-2018 19:46:41';
h5write(h5filename, '/creation_date', REF);
R = h5read(h5filename, '/creation_date');
asserts.isequal(R, REF);
%=============================================================================
REF = '';
h5write(h5filename, '/empty_str', REF);
R = h5read(h5filename, '/empty_str');
asserts.isequal(R, REF);
%=============================================================================
REF = 'c';
h5write(h5filename, '/one_char', REF);
R = h5read(h5filename, '/one_char');
asserts.isequal(R, REF);
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
REF = complexObj(3, 4);
h5write(h5filename, '/classdef_complex_obj', REF);
R = h5read(h5filename, '/classdef_complex_obj');
asserts.isequal(class(R), 'complexObj');
asserts.isequal(R.r, 3);
asserts.isequal(R.i, 4);
%=============================================================================
classdef_test_path = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(classdef_test_path);
REF = ClassdefPoint(7, 8);
h5write(h5filename, '/classdef_value', REF);
R = h5read(h5filename, '/classdef_value');
asserts.isequal(class(R), 'ClassdefPoint');
asserts.isequal(R.X, 7);
asserts.isequal(R.Y, 8);
asserts.isequal(R.Label, 'point');
%=============================================================================
counter = ClassdefCounter(11);
h5write(h5filename, '/classdef_handle', counter);
R = h5read(h5filename, '/classdef_handle');
asserts.isequal(class(R), 'ClassdefCounter');
asserts.istrue(ishandle(R));
asserts.istrue(isvalid(R));
asserts.isequal(R.Count, 11);
delete(counter);
delete(R);
%=============================================================================
if isfile(h5filename) rmfile(h5filename) end
%=============================================================================
