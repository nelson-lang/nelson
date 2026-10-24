%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);

filewrite([path_test, '/empty_swap_case.m'], ...
  ['function [y,x] = empty_swap_case(x,y)', newline()]);

filewrite([path_test, '/continued_comment_case.m'], ...
  ['function y = continued_comment_case()', newline(), ...
   'y = 1 + ... trailing text is ignored', newline(), ...
   '  2;', newline()]);

filewrite([path_test, '/continued_signature_case.m'], ...
  ['function y = continued_signature_case(a, ... trailing text is ignored', newline(), ...
   '  b)', newline(), ...
   'y = a + b;', newline()]);

filewrite([path_test, '/anonymous_cell_body_case.m'], ...
  ['function f = anonymous_cell_body_case(seed)', newline(), ...
   'f = {seed [1; 2] ... trailing text is ignored', newline(), ...
   '     @(pp) [pp 0]'' 1};', newline()]);

addpath(path_test);
clear functions;

[a, b] = empty_swap_case(2, 3);
asserts.isequal([a, b], [3, 2]);

asserts.isequal(continued_comment_case(), 3);

asserts.isequal(nargin('continued_signature_case'), 2);
asserts.isequal(continued_signature_case(2, 3), 5);

f = anonymous_cell_body_case(5);
asserts.isequal(f{1}, 5);
asserts.isequal(f{3}(7), [7; 0]);
asserts.isequal(f{4}, 1);
