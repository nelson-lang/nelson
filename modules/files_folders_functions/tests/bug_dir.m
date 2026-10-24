%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
__dir1 = tempname();
mkdir(__dir1);
c = onCleanup(@() rmdir(__dir1, 's')); %#ok<NASGU>
for k = 1:10
  file_name_to_create = [__dir1, '/file_', int2str(k)];
  if ~isfile(file_name_to_create)
    fp = fopen(file_name_to_create, 'wt');
    fclose(fp);
  end
end

A = dir(__dir1);
B = dir([__dir1, '/']);
asserts.isequal(A, B);
asserts.isequal(size(A), [12 1]);
%=============================================================================
