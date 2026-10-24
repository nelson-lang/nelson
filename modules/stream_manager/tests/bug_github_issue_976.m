%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/976
% <-- Short Description -->
% wrong output when reading a file with fscanf.
%=============================================================================
test_file = [tempname(), '.txt'];
fID1 = fopen(test_file, 'wt');
for n = 1:4
  b1 = n; b2 = n ^ 2; b3 = n ^ 3;
  fprintf(fID1, '%7u %7u %7u \r', b1, b2, b3);
end
fclose(fID1);

% then reopen it
fID1 = fopen(test_file, 'r');
for n = 1:4
  b1 = n; b2 = n ^ 2; b3 = n ^ 3;
  R = fscanf(fID1, '%7u %7u %7u \r', 3);
  asserts.isequal(R(1), b1);
  asserts.isequal(R(2), b2);
  asserts.isequal(R(3), b3);
end
fclose(fID1);
rmfile(test_file);
