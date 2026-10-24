%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.nc'];
A = reshape(1:262144, 512, 512);
nccreate(filename, 'x', 'Dimensions', {'row', 512, 'column', 512}, 'Datatype', 'double');
tic();
ncwrite(filename, 'x', A);
toc()
B = ncread(filename, 'x');
asserts.isequal(size(B), size(A));
asserts.isequal(B(1, 1), A(1, 1));
asserts.isequal(B(end, end), A(end, end));
%=============================================================================
