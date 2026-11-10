%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.nh5'];
x = rand(1000, 1000);
savenh5(filename, 'x');
m = nh5file(filename);
tic();
for k = 1:200
  r = m.x(k, :);
  b = m.x(k:k + 9, k:k + 9);
  s = size(m, 'x');
end
toc();
asserts.isequal(r, x(200, :));
asserts.isequal(b, x(200:209, 200:209));
asserts.isequal(s, [1000 1000]);
delete(filename);
%=============================================================================
