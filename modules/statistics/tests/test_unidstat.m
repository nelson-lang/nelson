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
asserts.isequal(nargin('unidstat'), 1);
asserts.isequal(nargout('unidstat'), 2);
%=============================================================================
[m, v] = unidstat(5);
asserts.isequal(m, 3);
asserts.isequal(v, 2);
%=============================================================================
[m, v] = unidstat([1 2 5 10]);
asserts.isequal(m, [1 1.5 3 5.5]);
asserts.isequal(v, [0 0.25 2 8.25]);
%=============================================================================
[m, v] = unidstat(single([2 5]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isequal(m, single([1.5 3]));
asserts.isequal(v, single([0.25 2]));
%=============================================================================
[m, v] = unidstat([0 -1 1.5 NaN]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
[m, v] = unidstat([]);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('unidstat()', _('Not enough input arguments.'));
asserts.checkerror('unidstat(1, 2)', _('Too many input arguments.'));
asserts.checkerror('unidstat({1})', _('N must be a positive integer numeric array.'));
%=============================================================================
