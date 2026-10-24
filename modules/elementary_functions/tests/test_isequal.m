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
asserts.checkerror('isequal()', _('Not enough input arguments.'));
asserts.checkerror('isequal(1)', _('Not enough input arguments.'));
asserts.checkerror('isequal([1, 1])', _('Not enough input arguments.'));
%=============================================================================
asserts.isequal(nargin('isequal'), -1);
asserts.isequal(nargout('isequal'), 1);
%=============================================================================
asserts.isequal(isequal(struct([]), struct([])), true);
asserts.isequal(isequal(struct('nelson', 1), struct('nelson', 1)), true);
asserts.isequal(isequal(struct('nelson', 1), struct('nelson', 2)), false);
asserts.isequal(isequal(struct('nelson', 1), struct('sonnel', 1)), false);
asserts.isequal(isequal(struct('nelson', 1, 'sonnel', 2), struct('nelson', 1, 'sonnel', 2)), true);
asserts.isequal(isequal(struct('nelson', 1, 'sonnel', 2), struct('sonnel', 2, 'nelson', 1)), true);
asserts.isequal(isequal(struct('nelson', 1, 'sonnel', 2), struct('nelson', 1, 'sonnel', 2), struct('nelson', 1, 'sonnel', 2)), true);
asserts.isequal(isequal(struct('nelson', 'abc', 'sonnel', 2), struct('nelson', 'abc', 'sonnel', 2)), true);
%=============================================================================
asserts.isequal(isequal(11, 11), true);
asserts.isequal(isequal([11, 11], [11, 11]), true);
asserts.isequal(isequal([11; 11; 11], [11; 11; 11]), true);
asserts.isequal(isequal(complex(11, 0), 11), true);
%=============================================================================
asserts.isequal(isequal([11, 21, 31, 41], [11, 21, 31, 41]), true);
asserts.isequal(isequal([11; 21; 31; 41], [11; 21; 31; 41]), true);
asserts.isequal(isequal([11, 21, 31, 41], [11; 21; 31; 41]), false);
asserts.isequal(isequal([11, 21, 31, 41], [11, 21; 31, 41]), false);
asserts.isequal(isequal([11, 21, 31, 41], [11, 31; 21, 41]), false);
%=============================================================================
asserts.isequal(isequal([], []), true);
asserts.isequal(isequal([], single([])), true);
%=============================================================================
asserts.isequal(isequal([], [], 1), false);
asserts.isequal(isequal([], 1, []), false);
asserts.isequal(isequal(1, [], []), false);
asserts.isequal(isequal(1, [], []), false);
%=============================================================================
asserts.isequal(isequal('nelson', 'nelson'), true);
asserts.isequal(isequal('nelson', 'sonnel'), false);
asserts.isequal(isequal('nelsonsonnelnelsonsonnel', ['nelson', 'sonnel', 'nelson', 'sonnel']), true);
asserts.isequal(isequal(['nelson', 'sonnel', 'son', 'nel'], ['nelson', 'sonnel', 'son', 'nel']), true);
asserts.isequal(isequal(['nelson', 'sonnel', 'son', 'e'], ['nelson', 'sonnel', 'son', 'nel']), false);
asserts.isequal(isequal(['TesT   '; 'sTring '], ['TesT   '; 'sTring '], ['TesT   '; 'sTring ']), true);
asserts.isequal(isequal(['nelson', 'sonnel', 'son', 'nel'], ['nelson'; 'sonnel'; 'son   '; 'nel   ']), false);
%=============================================================================
A = str2func('cos');
B = str2func('sin');
asserts.isequal(isequal(A, A), true);
asserts.isequal(isequal(A, B), false);
%=============================================================================
asserts.isequal(isequal(cell(11, 11), cell(11, 11)), true);
asserts.isequal(isequal(cell(11, 11), cell(11, 21)), false);
asserts.isequal(isequal({'nelson', 1}, {'nelson', 1}), true);
asserts.isequal(isequal({'nelson', 1}, {'nelson', 2}), false);
asserts.isequal(isequal({'nelson', 1}, {'sonnel', 1}), false);
asserts.isequal(isequal({'nelson', 1, 'sonnel', 2}, {'nelson', 1, 'sonnel', 2}), true);
asserts.isequal(isequal({'nelson', 1, 'sonnel', 2}, {'sonnel', 2, 'nelson', 1}), false);
asserts.isequal(isequal({'nelson', 1, 'sonnel', 2}, {'nelson', 1, 'sonnel', 2}, {'nelson', 1, 'sonnel', 2}), true);
asserts.isequal(isequal({'nelson', 'abc', 'sonnel', 2}, {'nelson', 'abc', 'sonnel', 2}), true);
asserts.isequal(isequal({'nelson', 'sonnel', 'son', 'nel'}, {'nelson', 'sonnel', 'son', 'nel'}), true);
asserts.isequal(isequal({'nelson', 'sonnel', 'son', 'nel'}, {'nelson'; 'sonnel'; 'son'; 'nel'}), false);
asserts.isequal(isequal(['nelson', 'sonnel', 'son', 'nel'], {'nelson', 'sonnel', 'son', 'nel'}), false);
%=============================================================================
asserts.isequal(isequal(sparse([]), []), true);
asserts.isequal(isequal([], sparse([])), true);
asserts.isequal(isequal(sparse([]), sparse([])), true);
asserts.isequal(isequal(sparse(0, 1), sparse(0, 1)), true);
asserts.isequal(isequal(sparse(0, 1), zeros(0, 1)), true);
asserts.isequal(isequal(sparse(2, 2), sparse(2, 2)), true);
asserts.isequal(isequal(zeros(2, 2), sparse(2, 2)), true);
asserts.isequal(isequal(sparse(0, 1), sparse(1, 0)), false)
%=============================================================================
