%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('class'), 2);
asserts.isequal(nargout('class'), 1);
%=============================================================================
A = 3;
res = class(A);
asserts.isequal(res, 'double');
%=============================================================================
C = [1; 3i];
res = class(C);
asserts.isequal(res, 'double');
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
c = complexObj(3, 4);
res = class(c);
asserts.isequal(res, 'complexObj');
%=============================================================================
A = sparse(2i);
res = class(A);
asserts.isequal(res, 'double');
asserts.istrue(issparse(A));
%=============================================================================
A = sparse(true);
res = class(A);
asserts.isequal(res, 'logical');
asserts.istrue(issparse(A));
%=============================================================================
A = ones(3, 3, 3);
res = class(A);
asserts.isequal(res, 'double');
%=============================================================================
A = {'r', 1};
res = class(A);
asserts.isequal(res, 'cell');
%=============================================================================
A = struct([]);
A.f1 = 3;
res = class(A);
asserts.isequal(res, 'struct');
%=============================================================================
A = single(i);
res = class(A);
asserts.isequal(res, 'single');
%=============================================================================
A = true;
res = class(A);
asserts.isequal(res, 'logical');
%=============================================================================
A = 'Nelson';
res = class(A);
asserts.isequal(res, 'char');
%=============================================================================
A = int8(3);
res = class(A);
asserts.isequal(res, 'int8');
%=============================================================================
A = int16(3);
res = class(A);
asserts.isequal(res, 'int16');
%=============================================================================
A = int32(3);
res = class(A);
asserts.isequal(res, 'int32');
%=============================================================================
A = int64(3);
res = class(A);
asserts.isequal(res, 'int64');
%=============================================================================
A = uint8(3);
res = class(A);
asserts.isequal(res, 'uint8');
%=============================================================================
A = uint16(3);
res = class(A);
asserts.isequal(res, 'uint16');
%=============================================================================
A = uint32(3);
res = class(A);
asserts.isequal(res, 'uint32');
%=============================================================================
A = uint64(3);
res = class(A);
asserts.isequal(res, 'uint64');
%=============================================================================
A = str2func('cos');
res = class(A);
asserts.isequal(res, 'function_handle');
%=============================================================================
asserts.checkerror('[a, b] = class(''a'')', _('Too many output arguments.'));
asserts.checkerror('r = class(''a'', 3, 4)', _('When called from outside a class constructor, the CLASS function must be called with one input.'));
%=============================================================================
