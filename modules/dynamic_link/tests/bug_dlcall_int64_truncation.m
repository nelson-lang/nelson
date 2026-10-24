%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% int64/uint64 FFI arguments were mapped to 32-bit FFI types and silently
% truncated: values above 2^32 lost their high word (2026-07 audit fix).
%=============================================================================
lib = dlopen([modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()]);
%=============================================================================
f = dlsym(lib, 'dynlibTestInputInt64', 'int64', {'int64'});
V = int64(4294967296); % 2^32
r = dlcall(f, V);
asserts.isequal(r, int64(4294967304)); % 2^32 + 8
%=============================================================================
f = dlsym(lib, 'dynlibTestInputUInt64', 'uint64', {'uint64'});
V = uint64(4294967296); % 2^32
r = dlcall(f, V);
asserts.isequal(r, uint64(4294967303)); % 2^32 + 7
%=============================================================================
% negative int64 below -2^32 must round-trip too
f = dlsym(lib, 'dynlibTestInputInt64', 'int64', {'int64'});
V = int64(-4294967296);
r = dlcall(f, V);
asserts.isequal(r, int64(-4294967288)); % -2^32 + 8
%=============================================================================
delete(lib);
%=============================================================================
