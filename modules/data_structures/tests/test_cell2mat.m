%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
REF = magic(6);
R = cell2mat(num2cell(REF));
asserts.isequal(R, REF);
%=============================================================================
C = {[10], [20 30 40]; [50; 90], [60 70 80; 100 110 120]};
R = cell2mat(C);
REF = [10 20 30 40;
  50 60 70 80;
  90 100 110 120];
asserts.isequal(R, REF);
%=============================================================================
rng('default');
M = rand(3, 3, 3);
C = num2cell(M);
R = cell2mat(C);
REF = M;
asserts.isapprox(R, REF, 1e-3);
%=============================================================================
M = [1 2; 3 4];
asserts.isequal(cell2mat(num2cell(M)), M);
BlockCell = {[10], [20 30 40]; [50; 90], [60 70 80; 100 110 120]};
asserts.isequal(cell2mat(BlockCell), [10 20 30 40; 50 60 70 80; 90 100 110 120]);
asserts.isequal(size(cell2mat({})), [0 0]);
LogicalMatrix = cell2mat({true, false; false, true});
asserts.isequal(LogicalMatrix, [true false; false true]);
asserts.isequal(class(LogicalMatrix), 'logical');
asserts.isequal(cell2mat({1, 'a'}), [char(1) 'a']);
%=============================================================================
CharMatrix = cell2mat({'ab', 'cd'; 'ef', 'gh'});
asserts.isequal(CharMatrix, ['abcd'; 'efgh']);
%=============================================================================
ComplexCell = {1 + 2i, 3 + 4i; 5 + 6i, 7 + 8i};
asserts.isequal(cell2mat(ComplexCell), [1 + 2i, 3 + 4i; 5 + 6i, 7 + 8i]);
%=============================================================================
NdBlocks = cell(1, 2, 2);
NdBlocks{1, 1, 1} = 1 * ones(2, 1, 1);
NdBlocks{1, 2, 1} = 2 * ones(2, 3, 1);
NdBlocks{1, 1, 2} = 3 * ones(2, 1, 2);
NdBlocks{1, 2, 2} = 4 * ones(2, 3, 2);
NdExpected = zeros(2, 4, 3);
NdExpected(:, 1, 1) = 1;
NdExpected(:, 2:4, 1) = 2;
NdExpected(:, 1, 2:3) = 3;
NdExpected(:, 2:4, 2:3) = 4;
asserts.isequal(cell2mat(NdBlocks), NdExpected);
%=============================================================================
asserts.isequal(cell2mat({[1 2], zeros(1, 0)}), [1 2]);
asserts.checkerror('cell2mat({[1 2], [3; 4]})', _('Dimensions of arrays being concatenated are not consistent.'));
%=============================================================================
% 0x0 elements are left out, mixed classes follow the concatenation rules
asserts.isequal(cell2mat({[1 2], []}), [1 2]);
asserts.isequal(cell2mat({[], [1 2]}), [1 2]);
asserts.isequal(cell2mat({[1 2]; []}), [1 2]);
asserts.isequal(cell2mat({[], []}), []);
asserts.isequal(cell2mat({'', ''}), '');
asserts.isequal(cell2mat({1, int8(2)}), int8([1 2]));
asserts.isequal(cell2mat({true, 2}), [1 2]);
asserts.isequal(cell2mat({single(1), 2}), single([1 2]));
asserts.isequal(cell2mat({1i, 2}), [1i 2]);
R = cell2mat({sparse(1), 2});
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1 2]);
R = cell2mat({sparse(true), sparse(false)});
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'logical');
%=============================================================================
