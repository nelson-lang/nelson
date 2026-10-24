%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Nelson displays logical values as the words true and false, never as 1 and 0
%=============================================================================
format('default');
asserts.isequal(formattedDisplayText(true), "     true" + newline);
asserts.isequal(formattedDisplayText([true false]), "     true    false" + newline);
asserts.isequal(formattedDisplayText(false(1, 0)), "");
%=============================================================================
R = evalc('x = true');
REF = sprintf('\nx =\n\n  logical\n\n     true\n\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('x = false');
REF = sprintf('\nx =\n\n  logical\n\n    false\n\n');
asserts.isequal(R, REF);
%=============================================================================
format('compact');
R = evalc('x = true');
REF = sprintf('x =\n  logical\n     true\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('y = [true false; false true]');
REF = sprintf('y =\n  2×2 logical array\n     true    false\n    false     true\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('w = true(2, 2, 2)');
REF = sprintf('  2×2×2 logical array\nw(:,:,1) =\n     true     true\n     true     true\nw(:,:,2) =\n     true     true\n     true     true\n');
asserts.isequal(R, REF);
%=============================================================================
format('+');
R = evalc('y = [true false; false true]');
REF = sprintf('y =\n  2×2 logical array\n+ \n +\n');
asserts.isequal(R, REF);
%=============================================================================
format('hex');
R = evalc('x = [true false]');
REF = sprintf('x =\n  1×2 logical array\n     true    false\n');
asserts.isequal(R, REF);
%=============================================================================
format('long');
R = evalc('x = [true false]');
asserts.isequal(R, REF);
%=============================================================================
format('short');
format('compact');
R = evalc('s = sparse(logical([1 0; 0 1]))');
REF = sprintf('s =\n  2×2 sparse logical array (2 nonzeros)\n   (1,1)   true\n   (2,2)   true\n');
asserts.isequal(R, REF);
%=============================================================================
% a wide row is split in column groups with a header
R = evalc('z = true(1, 30)');
asserts.istrue(contains(R, 'Columns 1 through'));
asserts.isfalse(contains(R, '   1'));
%=============================================================================
format('default');
