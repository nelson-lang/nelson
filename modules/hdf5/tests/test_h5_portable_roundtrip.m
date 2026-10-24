%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
h5filename = [tempname(), '.h5'];
if isfile(h5filename)
  rmfile(h5filename);
end
%=============================================================================
A = reshape(1:12, 3, 4);
h5write(h5filename, '/A', A);
R = h5read(h5filename, '/A');
asserts.isequal(R, A);
%=============================================================================
title = 'portable hdf5 roundtrip';
h5writeatt(h5filename, '/', 'title', title);
R = h5readatt(h5filename, '/', 'title');
asserts.isequal(R, title);
%=============================================================================
B = single(rand(2, 3, 4));
h5write(h5filename, '/B', B);
R = h5read(h5filename, '/B');
asserts.isapprox(R, B, 1e-7);
%=============================================================================
if isfile(h5filename)
  rmfile(h5filename);
end
%=============================================================================
