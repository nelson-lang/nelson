%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('fgets'), 2);
assert_isequal(nargout('fgets'), 1);
%=============================================================================
f = [nelsonroot(), '/etc/startup.m'];
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
assert_isequal(i,  26);
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
assert_isequal(i, 220);
%=============================================================================
