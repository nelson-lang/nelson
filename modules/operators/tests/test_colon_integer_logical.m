%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the step keeps its sign for unsigned integer ranges
R = uint8(1):-1:uint8(0);
asserts.isequal(R, uint8([1 0]));
asserts.isequal(class(R), 'uint8');
asserts.isequal(uint8(250):2:uint8(255), uint8([250 252 254]));
asserts.isequal(uint8(255):uint8(250), zeros(1, 0, 'uint8'));
asserts.isequal(int8(1):-1:int8(-2), int8([1 0 -1 -2]));
asserts.isequal(int8(-128):int8(-126), int8([-128 -127 -126]));
%=============================================================================
% logical operands count as doubles
asserts.isequal(true:3, [1 2 3]);
asserts.isequal(class(true:3), 'double');
asserts.isequal(1:true, 1);
asserts.isequal(true:true, 1);
asserts.isequal(false:true, [0 1]);
asserts.isequal(class(false:true), 'double');
%=============================================================================
% logical operands only combine with doubles and logicals
assert_checkerror('single(1):true', _('Colon operands must be all the same type, or mixed with real scalar doubles.'));
assert_checkerror('true:single(3)', _('Colon operands must be all the same type, or mixed with real scalar doubles.'));
assert_checkerror('uint8(1):1:true', _('Colon operands must be all the same type, or mixed with real scalar doubles.'));
assert_checkerror('int8(1):true', _('Colon operands must be all the same type, or mixed with real scalar doubles.'));
asserts.isequal(true:2:5, [1 3 5]);
asserts.isequal(class(true:2:5), 'double');
asserts.isequal(true:3.5, [1 2 3]);
%=============================================================================
% a double operand combined with an integer class is an integer value of that class
assert_checkerror('uint8(1):(-1)', _('Colon operands must be all the same type, or mixed with real double scalar.'));
assert_checkerror('uint8(1):NaN', _('Colon operands must be all the same type, or mixed with real double scalar.'));
assert_checkerror('int8(-128):Inf', _('Colon operands must be all the same type, or mixed with real double scalar.'));
assert_checkerror('uint8(1):300', _('Colon operands must be all the same type, or mixed with real double scalar.'));
assert_checkerror('uint8(1):1.5', _('Colon operands must be all the same type, or mixed with real double scalar.'));
assert_checkerror('uint8(1):1:-5', _('Colon operands must be all the same type, or mixed with real double scalar.'));
assert_checkerror('uint8(1):0.5:uint8(3)', _('Colon operands must be all the same type, or mixed with real double scalar.'));
asserts.isequal(uint8(1):1:5, uint8(1:5));
asserts.isequal(uint8(5):-2:1, uint8([5 3 1]));
asserts.isequal(uint8(250):-1000:uint8(3), uint8(250));
%=============================================================================
% an empty operand gives an empty range of the integer class
R = uint8(1):[];
asserts.isequal(size(R), [1 0]);
asserts.isequal(class(R), 'uint8');
R = int8(1):[]:int8(5);
asserts.isequal(size(R), [1 0]);
asserts.isequal(class(R), 'int8');
R = []:int8(3);
asserts.isequal(size(R), [1 0]);
asserts.isequal(class(R), 'int8');
%=============================================================================
% char ranges need char first and last operands
assert_checkerror('''a'':1:5', _('For colon operator with char operands, first and last operands must be char.'));
asserts.isequal('a':2:'e', 'ace');
%=============================================================================
