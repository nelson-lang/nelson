%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.mat'];
x = rand(1000, 1000);
savemat(filename, 'x', '-v7.3');
m = matfile(filename, 'Writable', true);
tic();
for k = 1:200
  m.x(k:k + 9, k:k + 9) = zeros(10);
end
for k = 1:20
  m.g(k, 1:100) = 1:100;
end
toc();
asserts.isequal(m.x(200:209, 200:209), zeros(10));
asserts.isequal(size(m, 'g'), [20 100]);
delete(filename);
%=============================================================================
