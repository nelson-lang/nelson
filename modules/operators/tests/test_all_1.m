%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('all'), 2)
asserts.isequal(nargout('all'), 1)
%=============================================================================
asserts.isequal(all([]), true)
asserts.isequal(all([], 1), logical(zeros(1, 0)))
asserts.isequal(all([], 2), logical(zeros(0, 1)))
asserts.isequal(all([], 3), logical(zeros(0, 0)))
%=============================================================================
asserts.isequal(all(ones(1, 0)), true)
asserts.isequal(all(ones(0, 1)), true)
%=============================================================================
asserts.isequal(all(ones(0, 0, 0)), logical(zeros(1, 0, 0)))
%=============================================================================
asserts.isequal(all(ones(0, 2)), [true true])
asserts.isequal(all(ones(3, 0)), logical(ones(1, 0)))
asserts.isequal(all(ones(3, 0, 1)), logical(ones(1, 0)))
%=============================================================================
asserts.isequal(all([0 0 3; 0 0 3; 0 0 3]), logical([0 0 1]))
%=============================================================================
asserts.isequal(all([0 0 3; 0 0 3; 0 0 3], 2), logical([0; 0; 0]))
%=============================================================================
R = all(ones(0, 1));
REF = true;
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(0, 1), 1);
REF = true;
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(0, 1), 2);
REF = logical(zeros(0, 1));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(0, 1), 3);
REF = logical(zeros(0, 1));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(1, 0));
REF = true;
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(1, 0), 1);
REF = logical(zeros(1, 0));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(1, 0), 2);
REF = true;
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(1, 0), 3);
REF = logical(zeros(1, 0));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(0, 3));
REF = [true, true, true];
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(0, 3), 1);
REF = [true, true, true];
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(0, 3), 2);
REF = logical(zeros(0, 1));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(0, 3), 3);
REF = logical(zeros(0, 3));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(3, 0));
REF = logical(zeros(1, 0));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(3, 0), 1);
REF = logical(zeros(1, 0));
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(3, 0), 2);
REF = [true; true; true];
asserts.isequal(R, REF);
%=============================================================================
R = all(ones(3, 0), 3);
REF = logical(zeros(3, 0));
asserts.isequal(R, REF);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'all');
asserts.checkerror('all(cell(0, 1))', msg);
%=============================================================================