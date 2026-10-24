%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
asserts.isequal(nargin('dlsym'), 4);
asserts.isequal(nargout('dlsym'), -1);
%=============================================================================
lib = dlopen([modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()]);
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarUInt8', 'void', {});
r = dlcall(f);
asserts.isequal(r, []);
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarUInt8', 'uint8', {});
r = dlcall(f);
asserts.isequal(r, uint8(8));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarInt8', 'int8', {});
r = dlcall(f);
asserts.isequal(r, int8(9));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarUInt16', 'uint16', {});
r = dlcall(f);
asserts.isequal(r, uint16(16));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarInt16', 'int16', {});
r = dlcall(f);
asserts.isequal(r, int16(17));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarUInt32', 'uint32', {});
r = dlcall(f);
asserts.isequal(r, uint32(32));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarInt32', 'int32', {});
r = dlcall(f);
asserts.isequal(r, int32(33));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarUInt64', 'uint64', {});
r = dlcall(f);
asserts.isequal(r, uint64(64));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarInt64', 'int64', {});
r = dlcall(f);
asserts.isequal(r, int64(65));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarDouble', 'double', {});
r = dlcall(f);
asserts.isequal(r, double(3.33));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeScalarSingle', 'float', {});
r = dlcall(f);
asserts.isequal(r, single(6.66));
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeCString', 'cstring', {});
r = dlcall(f);
asserts.isequal(r, 'hello utf-8');
%=============================================================================
f = dlsym(lib, 'dynlibTestReturnTypeWString', 'wstring', {});
r = dlcall(f);
asserts.isequal(r, 'hello unicode');
%=============================================================================
f = dlsym(lib, 'dynlibTestInputUInt8', 'uint8', {'uint8'});
V = uint8(1);
V_REF = uint8(1);
r = dlcall(f, V);
asserts.isequal(r, uint8(2));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputInt8', 'int8', {'int8'});
V = int8(1);
V_REF = int8(1);
r = dlcall(f, V);
asserts.isequal(r, int8(3));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputUInt16', 'uint16', {'uint16'});
V = uint16(1);
V_REF = uint16(1);
r = dlcall(f, V);
asserts.isequal(r, uint16(4));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputInt16', 'int16', {'int16'});
V = int16(1);
V_REF = int16(1);
r = dlcall(f, V);
asserts.isequal(r, int16(5));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputUInt32', 'uint32', {'uint32'});
V = uint32(1);
V_REF = uint32(1);
r = dlcall(f, V);
asserts.isequal(r, uint32(6));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputInt32', 'int32', {'int32'});
V = int32(1);
V_REF = int32(1);
r = dlcall(f, V);
asserts.isequal(r, int32(7));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputUInt64', 'uint64', {'uint64'});
V = uint64(1);
V_REF = uint64(1);
r = dlcall(f, V);
asserts.isequal(r, uint64(8));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputInt64', 'int64', {'int64'});
V = int64(1);
V_REF = int64(1);
r = dlcall(f, V);
asserts.isequal(r, int64(9));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputFloat', 'float', {'float'});
V = single(1);
V_REF = single(1);
r = dlcall(f, V);
asserts.isequal(r, single(10));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputDouble', 'double', {'double'});
V = double(1);
V_REF = double(1);
r = dlcall(f, V);
asserts.isequal(r, double(11));
asserts.isequal(V, V_REF);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputCString', 'cstring', {'cstring'});
for k = 1:10
  V = 'hello cstring';
  r = dlcall(f, V);
  asserts.isequal(r, 'HELLO CSTRING');
  asserts.isequal(V, 'hello cstring');
end
%=============================================================================
f = dlsym(lib, 'dynlibTestInputWString', 'wstring', {'wstring'});
for k = 1:10
  V = 'hello wstring';
  r = dlcall(f, V);
  asserts.isequal(r, 'HELLO WSTRING');
  asserts.isequal(V, 'hello wstring');
end
%=============================================================================
V = uint8([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputUInt8Ptr', 'void', {'uint8Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, uint8([1 2; 3 4] * 2));
asserts.isequal(V, uint8([1 2; 3 4]));
%=============================================================================
V = int8([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputInt8Ptr', 'void', {'int8Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, int8([1 2; 3 4] * 3));
asserts.isequal(V, int8([1 2; 3 4]));
%=============================================================================
V = uint16([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputUInt16Ptr', 'void', {'uint16Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, uint16([1 2; 3 4] * 4));
asserts.isequal(V, uint16([1 2; 3 4]));
%=============================================================================
V = int16([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputInt16Ptr', 'void', {'int16Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, int16([1 2; 3 4] * 5));
asserts.isequal(V, int16([1 2; 3 4]));
%=============================================================================
V = uint32([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputUInt32Ptr', 'void', {'uint32Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, uint32([1 2; 3 4] * 6));
asserts.isequal(V, uint32([1 2; 3 4]));
%=============================================================================
V = int32([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputInt32Ptr', 'void', {'int32Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, int32([1 2; 3 4] * 7));
asserts.isequal(V, int32([1 2; 3 4]));
%=============================================================================
V = uint64([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputUInt64Ptr', 'void', {'uint64Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, uint64([1 2; 3 4] * 8));
asserts.isequal(V, uint64([1 2; 3 4]));
%=============================================================================
V = int64([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputInt64Ptr', 'void', {'int64Ptr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, int64([1 2; 3 4] * 9));
asserts.isequal(V, int64([1 2; 3 4]));
%=============================================================================
V = single([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputFloatPtr', 'void', {'floatPtr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, single([1 2; 3 4] * 10));
asserts.isequal(V, single([1 2; 3 4]));
%=============================================================================
V = double([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestInputDoublePtr', 'void', {'doublePtr', 'int32'});
r = dlcall(f, V, int32(numel(V)));
asserts.isequal(r, double([1 2; 3 4] * 11));
asserts.isequal(V, double([1 2; 3 4]));
%=============================================================================
V = double([1 2; 3 4]);
f = dlsym(lib, 'dynlibTestMultiplyDoubleArrayWithReturn', 'int32', {'doublePtr', 'int32'});
[r1, r2] = dlcall(f, V, int32(numel(V)));
asserts.isequal(r2, double([1 2; 3 4] * 6));
asserts.isequal(r1, int32(44));
%=============================================================================
Y = [11 22; 33 44];
X = 3;
Z = 9;
f = dlsym(lib, 'sumDoubleRef', 'double', {'double', 'doublePtr', 'double'});
[S, Y2] = dlcall(f, X, Y, Z);
asserts.isequal(S, 23);
asserts.isequal(Y2, Y);
%=============================================================================
asserts.checkerror('S = dlcall(f, 3, int32(3), 4);', _('Invalid type for #2 argument: doublePtr expected.'));
asserts.checkerror('S = dlcall(f, 3);', _('Not enough input arguments.'));
%=============================================================================
