%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = bytecode_end_indexed_assign_fun(n)
  a = [];
  c = {};
  s = struct('f', {});
  for i = 1:n
    a(end + 1) = i;
    c{end + 1} = i;
    s(end + 1).f = i;
  end
  r.a = a;
  r.c = c;
  r.n = numel(s);
  r.last = s(end).f;
end
%=============================================================================
