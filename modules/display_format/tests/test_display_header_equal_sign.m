%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% "name = " keeps a trailing blank for strings, structs, missing values and
% objects, "name =" has none for numeric, char, logical, cell and function
% handle values. N-D pages of a cell or a string array follow the object form,
% an N-D struct array is named once.
%=============================================================================
format('compact')
%=============================================================================
R = evalc('a = 3');
assert_isequal(R, sprintf('a =\n     3\n'));
%=============================================================================
R = evalc('s = "hello"');
assert_isequal(R, sprintf('s = \n    "hello"\n'));
%=============================================================================
R = evalc('sa = ["a" "bb"]');
assert_isequal(R, sprintf('sa = \n  1×2 string array\n    "a"    "bb"\n'));
%=============================================================================
R = evalc('c = ''abc''');
assert_isequal(R, sprintf('c =\n    ''abc''\n'));
%=============================================================================
R = evalc('l = true');
assert_isequal(R, sprintf('l =\n  logical\n     true\n'));
%=============================================================================
R = evalc('ce = {1}');
assert_isequal(R, sprintf('ce =\n  1×1 cell array\n    {[1]}\n'));
%=============================================================================
R = evalc('st = struct(''f'', 1)');
assert_isequal(R, sprintf('st = \n  struct with fields:\n\n    f: 1\n'));
%=============================================================================
R = evalc('m = missing');
assert_isequal(R, sprintf('m = \n  missing\n    <missing>\n'));
%=============================================================================
R = evalc('fh = @sin');
assert_isequal(R, sprintf('fh =\n  function_handle with value:\n    @sin\n'));
%=============================================================================
R = evalc('mp = containers.Map()');
assert_isequal(strtok(R, newline), 'mp = ');
%=============================================================================
o = onCleanup(@() 1);
R = evalc('o');
assert_isequal(strtok(R, newline), 'o = ');
clear('o');
%=============================================================================
R = evalc('sn = struct(''f'', {1}); sn(:,:,2) = struct(''f'', 2)');
assert_isequal(strtok(R, newline), 'sn = ');
%=============================================================================
p = cat(3, [1 2], [3 4]);
R = evalc('p');
assert_istrue(contains(R, sprintf('p(:,:,1) =\n')));
R = evalc('disp(p)');
assert_istrue(contains(R, sprintf('(:,:,1) =\n')));
%=============================================================================
pc = cat(3, {1}, {2});
R = evalc('pc');
assert_istrue(contains(R, sprintf('pc(:,:,1) = \n')));
assert_istrue(contains(R, sprintf('pc(:,:,2) = \n')));
R = evalc('disp(pc)');
assert_istrue(contains(R, sprintf('(:,:,1) = \n')));
%=============================================================================
ps = cat(3, "a", "b");
R = evalc('ps');
assert_istrue(contains(R, sprintf('ps(:,:,1) = \n')));
R = evalc('disp(ps)');
assert_istrue(contains(R, sprintf('(:,:,2) = \n')));
%=============================================================================
format('loose')
R = evalc('s = "hello"');
assert_isequal(R, sprintf('\ns = \n\n    "hello"\n\n'));
R = evalc('a = 3');
assert_isequal(R, sprintf('\na =\n\n     3\n\n'));
%=============================================================================
