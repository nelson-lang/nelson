%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
%=============================================================================
addpath([nelsonroot(), '/modules/memory_manager/tests/']);
%=============================================================================
clear global bytecodeGlobalA
asserts.isfalse(isvar('global', 'bytecodeGlobalA'));
%=============================================================================
clear global bytecodeGlobalScalarA
global bytecodeGlobalScalarA
bytecodeGlobalScalarA = 3;
bytecode_global_scalar_increment(5);
asserts.isequal(bytecodeGlobalScalarA, 8);
clear global bytecodeGlobalScalarA
asserts.isfalse(isvar('global', 'bytecodeGlobalScalarA'));
%=============================================================================
clear global bytecodeGlobalVectorA
global bytecodeGlobalVectorA
bytecodeGlobalVectorA = zeros(1, 8);
bytecode_global_vector_writer(2, [10 20 30]);
asserts.isequal(bytecodeGlobalVectorA, [0 10 20 30 0 0 0 0]);
asserts.isequal(bytecode_global_vector_reader(2, 3), [10 20 30]);
bytecode_global_vector_writer(6, [40 50]);
asserts.isequal(bytecodeGlobalVectorA, [0 10 20 30 0 40 50 0]);
clear global bytecodeGlobalVectorA
asserts.isfalse(isvar('global', 'bytecodeGlobalVectorA'));
%=============================================================================
clear global bytecodeGlobalVectorA bytecodeGlobalVectorIndex
global bytecodeGlobalVectorA bytecodeGlobalVectorIndex
bytecodeGlobalVectorA = 1:64;
bytecodeGlobalVectorIndex = 1;
refVector = bytecodeGlobalVectorA;
refIndex = bytecodeGlobalVectorIndex;
refValue = 0;
for k = 1:3
  j = refIndex;
  block = refVector(j:j + 23);
  refVector(j + 24:j + 47) = block + k;
  refIndex = 1 + mod(j + 1, 16);
  refValue = refValue + refVector(j);
end
R = bytecode_global_vector_slice_transfer(3);
asserts.isequal(R, refValue);
asserts.isequal(bytecodeGlobalVectorIndex, refIndex);
asserts.isequal(bytecodeGlobalVectorA, refVector);
clear global bytecodeGlobalVectorA bytecodeGlobalVectorIndex
asserts.isfalse(isvar('global', 'bytecodeGlobalVectorA'));
asserts.isfalse(isvar('global', 'bytecodeGlobalVectorIndex'));
%=============================================================================
global bytecodeGlobalA
bytecodeGlobalA = 41;
asserts.istrue(isvar('global', 'bytecodeGlobalA'));
asserts.isequal(bytecode_global_reader(), 41);
bytecode_global_writer(99);
asserts.isequal(bytecodeGlobalA, 99);
asserts.isequal(bytecode_global_reader(), 99);
%=============================================================================
bytecode_global_writer(123);
asserts.isequal(bytecodeGlobalA, 123);
%=============================================================================
clear global bytecodeGlobalA
asserts.isfalse(isvar('global', 'bytecodeGlobalA'));
%=============================================================================
