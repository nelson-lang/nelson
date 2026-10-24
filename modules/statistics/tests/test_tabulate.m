%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('tabulate'), 1);
asserts.isequal(nargout('tabulate'), 1);
%=============================================================================
T = tabulate([1 3 3 4]);
REF = [1 1 25; 2 0 0; 3 2 50; 4 1 25];
asserts.isequal(T, REF);
%=============================================================================
T = tabulate([1.5 2.5 1.5 NaN]);
REF = [1.5 2 66.666666666666657; 2.5 1 33.333333333333329];
asserts.isapprox(T, REF, 1e-12);
%=============================================================================
T = tabulate(logical([true false true]));
asserts.isequal(T(:, 1), {false; true});
asserts.isequal(T{1, 2}, 1);
asserts.isequal(T{2, 2}, 2);
asserts.isapprox(cell2mat(T(:, 3)), [33.333333333333329; 66.666666666666657], 1e-12);
%=============================================================================
T = tabulate({'b', 'a', 'b'});
asserts.isequal(T(:, 1), {'b'; 'a'});
asserts.isequal(cell2mat(T(:, 2)), [2; 1]);
%=============================================================================
T = tabulate(["red"; "blue"; "red"]);
asserts.isequal(T(:, 1), {'red'; 'blue'});
asserts.isequal(cell2mat(T(:, 2)), [2; 1]);
%=============================================================================
C = categorical({'small', 'large', 'small'});
T = tabulate(C);
asserts.isequal(T(:, 1), {'small'; 'large'});
asserts.isequal(cell2mat(T(:, 2)), [2; 1]);
%=============================================================================
T = tabulate({'Male'; 'Male'; 'Female'; 'Female'; 'Female'});
asserts.isequal(T(:, 1), {'Male'; 'Female'});
asserts.isequal(cell2mat(T(:, 2)), [2; 3]);
asserts.isequal(cell2mat(T(:, 3)), [40; 60]);
%=============================================================================
asserts.checkerror('tabulate()', _('Not enough input arguments.'));
asserts.checkerror('tabulate({1, 2})', sprintf(_('%s must be a vector or text array.'), 'X'));
asserts.checkerror('tabulate([1 2; 3 4])', sprintf(_('%s must be a vector or text array.'), 'X'));
%=============================================================================
