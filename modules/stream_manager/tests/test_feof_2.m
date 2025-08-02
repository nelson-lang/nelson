%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [modulepath('characters_encoding', 'tests'), '/shisei_UTF-8.txt'];
fid = fopen(filename, 'rt', 'n', 'UTF-8');
assert_isequal(feof(fid), 0);
%=============================================================================
loop = true;
while loop
  r = feof(fid);
  if r == 0
    l = fgetl(fid);
  end
  loop = (r == 0);
end
assert_isequal(feof(fid), 1);
fclose(fid);
%=============================================================================
