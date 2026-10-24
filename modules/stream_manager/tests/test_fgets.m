%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('fgets'), 2);
asserts.isequal(nargout('fgets'), 1);
%=============================================================================
f = [tempname(), '.txt'];
filewrite(f, ['abcdef', newline(), 'ghijk', newline(), 'lmn']);
cleanupFile = onCleanup(@() rmfile(f)); %#ok<NLS0004>
%=============================================================================
fid = fopen(f, 'rt');
i = 0;
while ~feof(fid)
  tline = fgets(fid);
  if (~isnumeric(tline))
    %    disp(tline)
    i = i + 1;
  end
end
fclose(fid);
asserts.isequal(i, 3);
%=============================================================================
fid = fopen(f, 'rt');
i = 0;
while ~feof(fid)
  tline = fgets(fid, 5);
  if (~isnumeric(tline))
    %    disp(tline)
    i = i + 1;
  end
end
fclose(fid);
asserts.isequal(i, 5);
%=============================================================================
