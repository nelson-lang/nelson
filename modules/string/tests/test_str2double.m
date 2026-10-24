%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
asserts.isequal(nargin('str2double'), 1);
asserts.isequal(nargout('str2double'), 1);
%=============================================================================
asserts.isequal(str2double(''), NaN);
asserts.isequal(str2double('NaN'), NaN);
asserts.isequal(str2double('NAN'), NaN);
asserts.isequal(str2double('Nan'), NaN);
asserts.isequal(str2double('-NaN'), NaN);
asserts.isequal(str2double('-NAN'), NaN);
asserts.isequal(str2double('-Nan'), NaN);
asserts.isequal(str2double('+NaN'), NaN);
asserts.isequal(str2double('+NAN'), NaN);
asserts.isequal(str2double('+Nan'), NaN);
asserts.isequal(str2double('Inf'), Inf);
asserts.isequal(str2double('INf'), Inf);
asserts.isequal(str2double('+Inf'), Inf);
asserts.isequal(str2double('+INf'), Inf);
asserts.isequal(str2double('-Inf'), -Inf);
asserts.isequal(str2double('-INf'), -Inf);
%=============================================================================
asserts.isequal(str2double('10'), 10);
asserts.isequal(str2double('+10'), 10);
asserts.isequal(str2double('-10'), -10);
asserts.isequal(str2double('10.'), 10);
asserts.isequal(str2double('+10.'), 10);
asserts.isequal(str2double('-10.'), -10);
asserts.isequal(str2double('.4'), .4);
asserts.isequal(str2double('+.4'), .4);
asserts.isequal(str2double('-.44'), -.44);
asserts.isequal(str2double('10.4'), 10.4);
asserts.isequal(str2double('+10.44'), 10.44);
asserts.isequal(str2double('-10.44'), -10.44);
%=============================================================================
asserts.isequal(str2double('123.45e7'), 123.45e7);
asserts.isequal(str2double('3.14159'), 3.14159);
asserts.isequal(str2double('1,200.34'), 1200.34);
%=============================================================================
asserts.isequal(str2double('4e2'), 400);
asserts.isequal(str2double('-4e2'), -400);
asserts.isequal(str2double('+4e2'), 400);
asserts.isequal(str2double('0.4e2'), 40);
%=============================================================================
asserts.istrue(isnan(str2double('3d2')));
asserts.istrue(isnan(str2double('3D2')));
asserts.isequal(str2double('3e2'), 300);
asserts.isequal(str2double('3E2'), 300);
%=============================================================================
asserts.isequal(str2double('3 2'), NaN);
asserts.isequal(str2double('  2'), 2);
asserts.isequal(str2double('2  '), 2);
asserts.isequal(str2double('  2  '), 2);
%=============================================================================
asserts.isequal(str2double({'3', 3; 5, '5'}), [3, NaN; NaN, 5]);
%=============================================================================
% non-text input is NaN, not an error
asserts.istrue(isnan(str2double(5)));
asserts.istrue(isnan(str2double(true)));
asserts.istrue(isnan(str2double([1 2])));
asserts.istrue(isnan(str2double({5})));
asserts.istrue(isnan(str2double(struct())));
asserts.istrue(isnan(str2double(@sin)));
R = str2double({'1', 5});
asserts.istrue(R(1) == 1 && isnan(R(2)));
%=============================================================================
% the imaginary term may come first, an operator may be followed by a sign
asserts.isequal(str2double('1i+1'), 1 + 1i);
asserts.isequal(str2double('2i-3'), -3 + 2i);
asserts.isequal(str2double('2i - 3'), -3 + 2i);
asserts.isequal(str2double('i+1'), 1 + 1i);
asserts.isequal(str2double('-i+1'), 1 - 1i);
asserts.isequal(str2double('1e3i+1e2'), 100 + 1000i);
asserts.isequal(str2double('1+-1i'), 1 - 1i);
asserts.isequal(str2double('1--1i'), 1 + 1i);
asserts.isequal(str2double('-1+-1i'), -1 - 1i);
asserts.isequal(str2double('1 + i'), 1 + 1i);
asserts.isequal(str2double('1 i'), 1i);
asserts.isequal(str2double('5*i'), 5i);
asserts.isequal(str2double('i*5'), 5i);
asserts.istrue(isnan(str2double('1+--1i')));
asserts.istrue(isnan(str2double('1i+1i')));
asserts.istrue(isnan(str2double('1+1i+1i')));
asserts.istrue(isnan(str2double('1+1')));
asserts.istrue(isnan(str2double('5*1i')));
asserts.istrue(isnan(str2double('1 2')));
%=============================================================================
% Inf and NaN parts
asserts.isequal(str2double('Inf+1i'), complex(Inf, 1));
R = str2double('NaN+1i');
asserts.istrue(isnan(real(R)) && imag(R) == 1);
R = str2double('1+NaNi');
asserts.istrue(real(R) == 1 && isnan(imag(R)));
R = str2double('NaNi');
asserts.istrue(real(R) == 0 && isnan(imag(R)));
%=============================================================================
% only Inf and NaN are words, no D exponent, hexadecimal floating point
asserts.istrue(isnan(str2double('Infinity')));
asserts.istrue(isnan(str2double('-Infinity')));
asserts.istrue(isnan(str2double('1d3')));
asserts.istrue(isnan(str2double('1D3')));
asserts.isequal(str2double('0x10'), 16);
asserts.isequal(str2double('0x1p3'), 8);
asserts.isequal(str2double('0x1p-3'), 0.125);
asserts.isequal(str2double('0x1.8p3'), 12);
asserts.isequal(str2double('0x1.8'), 1.5);
asserts.isequal(str2double('0x10i'), 16i);
asserts.isequal(str2double('0x10+1i'), 16 + 1i);
%=============================================================================
% surrounding whitespace of any kind
asserts.isequal(str2double(sprintf('5\n')), 5);
asserts.isequal(str2double(sprintf('\t5\t')), 5);
asserts.isequal(str2double(sprintf('5\r')), 5);
asserts.isequal(str2double(char([53 8192])), 5);
asserts.isequal(str2double(char([53 12288])), 5);
%=============================================================================
