%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
asserts.isequal(bytecode_local_scalar_plus(10), 55);
asserts.isequal(bytecode_local_scalar_plus_reversed(10), 55);
asserts.isequal(bytecode_local_scalar_plus_constant(10), 20);
asserts.istrue(abs(bytecode_local_scalar_plus_expression(10) - sum(1 ./ (1:10))) < 1e-12);
asserts.istrue(abs(bytecode_local_scalar_plus_local_rdiv(10) - sum((3:12) ./ (2:11))) < 1e-12);
R = bytecode_local_scalar_plus_local_rdiv_single(single(3), single(2));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single(1.5));
D = [1 2; 3 5];
failed = false;
try
  bytecode_local_scalar_plus_rdiv_matrix(D);
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(bytecode_local_scalar_plus_indexed(1:10), 55);
asserts.checkerror('bytecode_local_scalar_plus_undefined_indexed()', sprintf(_('Undefined variable: %s'), 'r'));
R = bytecode_local_scalar_plus_single(single(1), single(2));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single(3));
R = bytecode_local_scalar_plus_constant_single(single(1));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single(3));
%=============================================================================
